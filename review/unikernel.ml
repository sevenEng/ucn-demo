(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt
open V1_LWT

let () = Log.(set_log_level INFO)

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

  let get_time () =
    let open Clock in
    let tm = time () |> gmtime in
    Printf.sprintf "%d:%d:%d" tm.tm_hour tm.tm_min tm.tm_sec


  let headers =
    let hdr = Cohttp.Header.init () in
    Cohttp.Header.add hdr "Access-Control-Allow-Origin" "*" (* make chrome happy *)

  let respond_error status info =
    Log.info "%s %s" (get_time ()) info;
    S.respond_error ~headers ~status ~body:info ()


  let list_reviews s body =
    let t = "list reviewed movie ids" in
    let () = Log.info "%s %s" (get_time ()) t in
    let s = s t in
    match%lwt Store.read s ["list"] with
    | None -> respond_error `Not_implemented "no content"
    | Some s ->
       let () = Log.info "%s %s" (get_time ()) ("respond " ^ s) in
       let body = Cohttp_lwt_body.of_string s in
       S.respond ~headers ~status:`OK ~body ()


  let add_to_reviewed s id =
    let t = Printf.sprintf "add movie %s to reviewed" id in
    let () = Log.info "%s %s" (get_time ()) t in
    match%lwt Store.read s ["list"] with
    | None -> Lwt.fail_with "/list endpoint not initiated"
    | Some v ->
       match Ezjsonm.from_string v with
       | `A v_lst ->
          List.map Ezjsonm.get_string v_lst
          |> fun lst -> begin
                 if List.mem id lst then lst
                 else id :: lst end
          |> List.map Ezjsonm.string
          |> fun new_v_lst ->
          Store.update s ["list"] (Ezjsonm.to_string (`A new_v_lst))
       | _ -> Lwt.fail_with "not a json array"


  let delete_from_reviewed s id =
    let print_id_lst lst =
      Log.info "%s %s" (get_time ()) (String.concat " " lst);
      lst in
    let t = Printf.sprintf "remove movie %s from reviewed" id in
    let () = Log.info "%s %s" (get_time ()) t in
    match%lwt Store.read s ["list"] with
    | None -> Lwt.fail_with "/list endpoint not initiated"
    | Some v ->
       match Ezjsonm.from_string v with
       | `A v_lst ->
          v_lst
          |> List.map Ezjsonm.get_string
          |> print_id_lst
          |> List.filter (fun x -> not (x = id))
          |> print_id_lst
          |> List.map Ezjsonm.string
          |> fun new_v_lst ->
          Store.update s ["list"] (Ezjsonm.to_string (`A new_v_lst))
       | _ -> Lwt.fail_with "not a json array"


  let create_review path s body =
    let id = List.hd path in
    let t = "create new review " ^ id in
    let () = Log.info "%s %s" (get_time ()) t in
    let s = s t in
    Cohttp_lwt_body.to_string body
    >>= Store.update s [id]
    >>= fun () -> add_to_reviewed s id
    >>= fun () -> S.respond ~headers ~status:`OK ~body:Cohttp_lwt_body.empty ()


  let update_review path s body =
    let id = List.hd path in
    let t = "update a review " ^ id in
    let () = Log.info "%s %s" (get_time ()) t in
    let s = s t in
    Cohttp_lwt_body.to_string body
    >>= Store.update s [id]
    >>= S.respond ~headers ~status:`OK ~body:Cohttp_lwt_body.empty


  let read_review path s body =
    let id = List.hd path in
    let t = "read a review " ^ id in
    let () = Log.info "%s %s" (get_time ()) t in
    let s = s t in
    match%lwt Store.read s [id] with
    | None -> respond_error `Not_found ("no review for " ^ id)
    | Some v ->
       let () = Log.info "%s %s" (get_time ()) ("respond: " ^ v) in
       let body = Cohttp_lwt_body.of_string v in
       S.respond ~headers ~status:`OK ~body ()


  let delete_review path s body =
    let id = List.hd path in
    let t = "delete a review " ^ id in
    let () = Log.info "%s %s" (get_time ()) t in
    let s = s t in
    Store.remove s [id]
    >>= fun () -> delete_from_reviewed s id
    >>= S.respond ~headers ~status:`OK ~body:Cohttp_lwt_body.empty


  let meta_of_review v =
    match Ezjsonm.from_string v with
    | `O obj ->
       let title = List.assoc "title" obj in
       let meta = ["source", `String "review"; "title", title] in
       return Ezjsonm.(dict meta |> to_string)
    | _ -> Lwt.fail_with ("not a review object: " ^ v)


  let read_meta_review path s body =
    let id = List.hd path in
    let t = "create meta data for a review " ^ id in
    let () = Log.info "%s %s" (get_time ()) t in
    let s = s t in
    match%lwt Store.read s [id] with
    | None -> respond_error `Not_found ("no review for " ^ id)
    | Some v ->
       meta_of_review v >>= fun meta ->
       let () = Log.info "%s %s" (get_time ()) ("sending meta info:\n" ^ meta) in
       let body = Cohttp_lwt_body.of_string meta in
       S.respond ~headers ~status:`OK ~body ()


  let dispatch request =
    let path = Cohttp.Request.uri request |> Uri.path |> split_path in
    match path with
    | "create" :: rst -> create_review rst
    | "read" :: rst -> read_review rst
    | "update" :: rst -> update_review rst
    | "delete" :: rst -> delete_review rst
    | "meta" :: rst -> read_meta_review rst
    | ["list"] -> list_reviews
    | _ ->
       fun _ _ ->
       respond_error `Bad_request ("no endpoint for " ^ String.concat "/" path)


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


  let init_db s =
    let t = "init db with '/list' -> [] " in
    let () = Log.info "%s %s" (get_time ()) t in
    let s = s t in
    let v = Ezjsonm.to_string (`A []) in
    Store.update s ["list"]  v


  let start stack conf _clock () =
    (*
    X509.certificate conf `Default >>= fun cert ->
    let tls_config = Tls.Config.server ~certificates:(`Single cert) () in
    *)
    Store.Repo.create config >>= fun repo ->
    Store.master task repo >>= fun s ->
    init_db s >>= fun () ->
    let http = S.make ~conn_closed:ignore ~callback:(handle_request s) () in
    Stack.listen_tcpv4 stack ~port:8443 (S.listen http);
    Stack.listen stack
end
