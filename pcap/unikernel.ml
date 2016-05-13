(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt
open V1_LWT

let () = Log.(set_log_level INFO)

let irmin_uri = Uri.of_string "https://127.0.0.1:8444"

(* Never used, but needed to create the store. *)
let task s = Irmin.Task.create ~date:0L ~owner:"Server" s

module Context = struct
  let v () = failwith "Context"
end

module Mirage_git_memory = Irmin_mirage.Irmin_git.Memory(Context)(Git.Inflate.None)
module Store = Mirage_git_memory(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)
let config = Irmin_git.config ()

module Bundle = Tc.Pair(Store.Private.Slice)(Store.Hash)

(* Split a URI into a list of path segments *)
let split_path path =
  let rec aux = function
    | [] | [""] -> []
    | hd::tl -> hd :: aux tl
  in
  List.filter (fun e -> e <> "")
    (aux (Re_str.(split_delim (regexp_string "/") path)))

module Main (Stack:STACKV4) (Conf:KV_RO) (Clock:V1.CLOCK) = struct
  module TCP  = Stack.TCPV4
  (*
  module TLS  = Tls_mirage.Make (TCP)
  module X509 = Tls_mirage.X509 (Conf) (Clock)
  *)
  module S = Cohttp_mirage.Server(TCP)

  (* Take a new raw flow, perform a TLS handshake to get a TLS flow and call [f tls_flow].
     When done, the underlying flow will be closed in all cases. *)
  (*
  let wrap_tls tls_config f flow =
    let peer, port = TCP.get_dest flow in
    Log.info "Connection from %s (client port %d)" (Ipaddr.V4.to_string peer) port;
    TLS.server_of_flow tls_config flow >>= function
    | `Error _ -> Log.warn "TLS failed"; TCP.close flow
    | `Eof     -> Log.warn "TLS eof"; TCP.close flow
    | `Ok flow  ->
        Lwt.finalize
          (fun () -> f flow)
          (fun () -> TLS.close flow)
   *)

  type pcap = {
      hdr: Cstruct.t;
      packets: (Cstruct.t * Cstruct.t) list;
    }


  let get_time () =
    let open Clock in
    let tm = time () |> gmtime in
    Printf.sprintf "%d:%d:%d" tm.tm_hour tm.tm_min tm.tm_sec


  let cstruct_of_body body =
    Cohttp_lwt_body.to_string body >|= Cstruct.of_string


  let pcap_of_cstruct c =
    try match Pcap.detect c with
        | None -> failwith "no header detected"
        | Some h ->
           let hdr, body = Cstruct.split c Pcap.sizeof_pcap_header in
           let pkt_iter = Pcap.packets h body in
           let packets = Cstruct.fold (fun acc pkt -> pkt :: acc) pkt_iter [] in
           return_some (h, {hdr; packets})
    with _ as e ->
         Log.info "%s %s" (get_time ())
           (match e with
              Failure info -> Printf.sprintf "failwith %s" info
            | Invalid_argument info -> Printf.sprintf "invalid argument %s" info
            | _ -> "unknow exception");
         return_none


  let respond_error status info =
    Log.info "%s %s" (get_time ()) info;
    S.respond_error ~status ~body:info ()


  let read_from path s _ =
    let t = "read from " ^ (String.concat "/" path) in
    let () = Log.info "%s %s" (get_time ()) t in
    let s = s t in
    match%lwt Store.read s path with
    | None -> respond_error `Not_found ""
    | Some s ->
       let body = Cohttp_lwt_body.of_string s in
       S.respond ~status:`OK ~body ()


  let json_of_pcap (h, pcap) =
    let module Hdr = (val h : Pcap.HDR) in
    let open Ezjsonm in
    let hdr_v = `O [
        "magic_number", pcap.hdr |> Hdr.get_pcap_header_magic_number |> int32;
        "version_major", pcap.hdr |> Hdr.get_pcap_header_version_major |> int;
        "version_minor", pcap.hdr |> Hdr.get_pcap_header_version_minor |> int;
        "thiszone", pcap.hdr |> Hdr.get_pcap_header_thiszone |> int32;
        "sigfigs", pcap.hdr |> Hdr.get_pcap_header_sigfigs |> int32;
        "snaplen", pcap.hdr |> Hdr.get_pcap_header_snaplen |> int32;
        "network", pcap.hdr |> Hdr.get_pcap_header_network |> int32 ] in
    let f (pkt_hdr, pkt) =
      let hdr_v = `O [
          "ts_sec", pkt_hdr |> Hdr.get_pcap_packet_ts_sec |> int32;
          "ts_usec", pkt_hdr |> Hdr.get_pcap_packet_ts_usec |> int32;
          "incl_len", pkt_hdr |> Hdr.get_pcap_packet_incl_len |> int32;
          "orig_len", pkt_hdr |> Hdr.get_pcap_packet_orig_len |> int32;] in
      let pkt_v = `O ["data", pkt |> Cstruct.to_string |> string] in
      `O ["packet_hdr", hdr_v; "packet_data", pkt_v] in
    `O [
       "header", hdr_v;
       "packets", `A (List.map f pcap.packets);]


  let value_of_body raw =
    raw |> cstruct_of_body
    >>= pcap_of_cstruct
    >>= function
    | None -> return_none
    | Some s ->
       s |> json_of_pcap
       |> Ezjsonm.to_string ~minify:true
       |> return_some


  let write_to path s raw =
    let t = "write to " ^ (String.concat "/" path) in
    let () = Log.info "%s %s" (get_time ()) t in
    let s = s t in
    raw |> value_of_body
    >>= function
    | None -> respond_error `Bad_request "can't make sense of the body"
    | Some v ->
       Store.update s path v
       >>= fun () ->
       Log.info "%s write successed" (get_time ());
       S.respond ~status:`Created ~body:Cohttp_lwt_body.empty ()


  let dispatch request =
    let path = Cohttp.Request.uri request |> Uri.path |> split_path in
    let meth = Cohttp.Request.meth request in
    match meth, path with
    | _, [] ->
       fun _ _ -> respond_error `Bad_request "no action specifed"
    | `GET, "read" :: path -> read_from path
    | `POST, "write" :: path -> write_to path
    | _, _ ->
       fun _ _ ->
       respond_error `Bad_request "unrecognised method action pair {GET * 'read'| POST * 'write'}"


  let handle_request s (flow, conn) request body =
    let ip, port = TCP.get_dest flow in
    Log.info "%s connection %s from %s:%d"
             (get_time ())
             (Cohttp.Connection.to_string conn)
             (Ipaddr.V4.to_string ip) port;
    dispatch request s body


  let dump s =
    let s = s "export" in
    Store.head s >>= function
    | None -> failwith "dump: no head!"
    | Some head ->
    Store.Repo.export ~max:[head] (Store.repo s) >>= fun slice ->
    let bundle = (slice, head) in
    let buf = Cstruct.create (Bundle.size_of bundle) in
    let rest = Bundle.write bundle buf in
    assert (Cstruct.len rest = 0);
    let path = "init_db.ml" in
    Printf.printf "Writing %s...\n%!" path;
    let ch = open_out_bin path in
    Printf.fprintf ch "let init_db = %S" (Cstruct.to_string buf);
    close_out ch;
    Printf.printf "Wrote %s\n%!" path;
    return_unit


  let import s db =
    let s = s "import" in
    let buf = Mstruct.of_string db in
    let (slice, head) = Bundle.read buf in
    Store.Repo.import (Store.repo s) slice >>= function
    | `Error -> failwith "Irmin import failed"
    | `Ok ->
    Store.fast_forward_head s head >>= function
    | false -> failwith "Irmin import failed at FF"
    | true -> return ()


  let start stack conf _clock () =
    (*
    X509.certificate conf `Default >>= fun cert ->
    let tls_config = Tls.Config.server ~certificates:(`Single cert) () in
    *)
    Store.Repo.create config >>= fun repo ->
    Store.master task repo >>= fun s ->
    let http = S.make ~conn_closed:ignore ~callback:(handle_request s) () in
    Stack.listen_tcpv4 stack ~port:8433 (S.listen http);
    Stack.listen stack
end
