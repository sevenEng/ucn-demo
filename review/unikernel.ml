open Lwt
open V1_LWT


module Main (Stack:STACKV4) (Clock:V1.CLOCK) = struct

  module TCP  = Stack.TCPV4
  module Server = Cohttp_mirage.Server(TCP)

  module Context = struct let v () = return_none end
  module Mirage_git_memory = Irmin_mirage.Irmin_git.Memory(Context)(Git.Inflate.None)
  module Store = Mirage_git_memory(Irmin.Contents.Json)(Irmin.Ref.String)(Irmin.Hash.SHA1)

  let owner = "ucn.review"
  let task s = Irmin.Task.create ~date:0L ~owner s
  let config = Irmin_mem.config ()

  let src = Logs.Src.create owner
  module Log = (val Logs.src_log src : Logs.LOG)
  module Logs_reporter = Mirage_logs.Make(Clock)

  let headers =
    let hdr = Cohttp.Header.init () in
    Cohttp.Header.add hdr "Access-Control-Allow-Origin" "*" (* make chrome happy *)

  let data_path = ["data"]
  let log_path = ["logs"]

  let list_reviews s body =
    let t = "list id of reviewed movies" in
    Log.app (fun msgf -> msgf "%s" t);
    let s = s t in
    let p = data_path in
    Store.list s p >>= fun keys ->
    let id_lst = List.map (fun key -> key |> List.rev |> List.hd) keys in
    Log.info (fun msgf -> msgf "respond: [%s]" (String.concat " " id_lst));
    let arr = Ezjsonm.(list string id_lst) in
    let body = Cohttp_lwt_body.of_string (Ezjsonm.to_string arr) in
    Server.respond ~headers ~status:`OK ~body ()


  let check_fields = function
    | `O _ as d ->
       let l = Ezjsonm.get_dict d in
       (List.mem_assoc "id" l
        && List.mem_assoc "title" l
        && List.mem_assoc "rating" l
        && List.mem_assoc "comment" l)
       |> ignore;
       return_unit
    | _ as v ->
       fail_invalid_arg "not json object"


  let update_review path s body =
    let id = List.hd path in
    let t = "create/update new review " ^ id in
    Log.app (fun msgf -> msgf "%s" t);
    let s = s t in
    let p = data_path @ [id] in
    catch (fun () ->
      Cohttp_lwt_body.to_string body >>= fun body ->
      let v = Ezjsonm.from_string body in
      check_fields v >>= fun () ->
      Store.update s p v >>= fun () ->
      Log.info (fun msgf -> msgf "review created/updated");
      Server.respond ~status:`OK ~headers ~body:Cohttp_lwt_body.empty ()
      ) (fun e ->
      Log.app (fun msgf -> msgf "exn: %s" (Printexc.to_string e));
      Server.respond ~status:`Bad_request ~headers ~body:Cohttp_lwt_body.empty ())


  let read_review path s body =
    let id = List.hd path in
    let t = "read review " ^ id in
    Log.app (fun msgf -> msgf "%s" t);
    let s = s t in
    let p = data_path @ [id] in
    Store.read s p >>= function
    | None ->
       Log.info (fun msgf -> msgf "not found review %s" id);
       Server.respond_error ~headers ~status:`Not_found ~body:"" ()
    | Some v ->
       let v = Ezjsonm.to_string v in
       Log.info (fun msgf -> msgf "respond: %s" v);
       let body = Cohttp_lwt_body.of_string v in
       Server.respond ~headers ~status:`OK ~body ()


  let remove_review path s body =
    let id = List.hd path in
    let t = "remove review " ^ id in
    Log.app (fun msgf -> msgf "%s" t);
    let s = s t in
    let p = data_path @ [id] in
    Store.remove s p >>= fun () ->
    Log.info (fun msgf -> msgf "remove review %s" id);
    Server.respond ~headers ~status:`OK ~body:Cohttp_lwt_body.empty ()


  let meta_of_review = function
    | `O obj ->
       let title = List.assoc "title" obj in
       let meta = ["source", `String "review"; "title", title] in
       return Ezjsonm.(dict meta |> to_string)
    | _ -> Lwt.fail_with "never"


  let read_meta_review path s body =
    let id = List.hd path in
    let t = "read metadata of a review " ^ id in
    Log.app (fun msgf -> msgf "%s" t);
    let s = s t in
    let p = data_path @ [id] in
    Store.read s p >>= function
    | None ->
       Log.info (fun msgf -> msgf "not found review %s" id);
       Server.respond_error ~headers ~status:`Not_found ~body:"" ()
    | Some v ->
       meta_of_review v >>= fun meta ->
       Log.info (fun msgf -> msgf "respond metadata %s" meta);
       let body = Cohttp_lwt_body.of_string meta in
       Server.respond ~headers ~status:`OK ~body ()


  let dispatch request =
    let path =
      Cohttp.Request.uri request
      |> Uri.path
      |> Astring.String.cuts ~empty:false ~sep:"/" in
    match path with
    | "create" :: tl -> update_review tl
    | "read" :: tl -> read_review tl
    | "update" :: tl -> update_review tl
    | "delete" :: tl -> remove_review tl
    | "meta" :: tl -> read_meta_review tl
    | ["list"] -> list_reviews
    | _ ->
       fun _ _ ->
       Server.respond_error ~status:`Bad_request ~headers ~body:"" ()


  let handle_request s (flow, conn) request body =
    let ip, port = TCP.get_dest flow in
    Log.app (fun msgf -> msgf "connection %s from %s:%d"
      (Cohttp.Connection.to_string conn) (Ipaddr.V4.to_string ip) port);
    dispatch request s body


  let start stack clock () =
    (*Logs.set_reporter (Utils.src_stamp_reporter ());
    Logs.set_level (Some Logs.Info);*)
    Logs_reporter.(create () |> set_reporter);
    Store.Repo.create config >>= fun repo ->
    Store.master task repo >>= fun s ->
    let http = Server.make ~conn_closed:ignore ~callback:(handle_request s) () in
    Stack.listen_tcpv4 stack ~port:8081 (Server.listen http);
    Stack.listen stack
end
