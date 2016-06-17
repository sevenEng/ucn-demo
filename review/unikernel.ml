open Lwt
open V1_LWT


module Main (Stack:STACKV4) (Conf:KV_RO) (Clock:V1.CLOCK) = struct

  module TCP  = Stack.TCPV4
  module S = Cohttp_mirage.Server(TCP)

  module Context = struct let v () = return_none end
  module Mirage_git_memory = Irmin_mirage.Irmin_git.Memory(Context)(Git.Inflate.None)
  module Store = Mirage_git_memory(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

  let owner = "ucn.review"
  let task s = Irmin.Task.create ~date:0L ~owner s
  let config = Irmin_git.config ()

  let src = Logs.Src.create owner
  let stamp : Mtime.span Logs.Tag.def = Logs.Tag.def "stamp.tag" Mtime.pp_span

  module Log = struct
    let counter = Mtime.counter ()
    let stamp_tag () =
      Logs.Tag.add stamp (Mtime.count counter) Logs.Tag.empty

    let app f = Logs.msg ~src Logs.App (fun m -> f (m ~tags:(stamp_tag ())))
    let err f = Logs.msg ~src Logs.Error (fun m -> f (m ~tags:(stamp_tag ())))
    let warn f = Logs.msg ~src Logs.Warning (fun m -> f (m ~tags:(stamp_tag ())))
    let info f = Logs.msg ~src Logs.Info (fun m -> f (m ~tags:(stamp_tag ())))
    let debug f = Logs.msg ~src Logs.Debug (fun m -> f (m ~tags:(stamp_tag ())))
  end


  let split_path path =
    let rec aux = function
      | [] | [""] -> []
      | hd::tl -> hd :: aux tl
    in
    List.filter (fun e -> e <> "")
                (aux (Re_str.(split_delim (regexp_string "/") path)))

  let headers =
    let hdr = Cohttp.Header.init () in
    Cohttp.Header.add hdr "Access-Control-Allow-Origin" "*" (* make chrome happy *)

  let respond_error status info =
    Log.info (fun msgf -> msgf "%s" info);
    S.respond_error ~headers ~status ~body:info ()


  let list_reviews s body =
    let t = "list reviewed movie ids" in
    Log.info (fun msgf -> msgf "%s" t);
    let s = s t in
    match%lwt Store.read s ["list"] with
    | None -> respond_error `Not_implemented "no content"
    | Some s ->
       Log.info (fun msgf -> msgf "%s" ("respond " ^ s));
       let body = Cohttp_lwt_body.of_string s in
       S.respond ~headers ~status:`OK ~body ()


  let add_to_reviewed s id =
    let t = Printf.sprintf "add movie %s to reviewed" id in
    Log.info (fun msgf -> msgf "%s" t);
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
      Log.info (fun msgf -> msgf "%s" (String.concat " " lst));
      lst in
    let t = Printf.sprintf "remove movie %s from reviewed" id in
    Log.info (fun msgf -> msgf "%s" t);
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
    Log.info (fun msgf -> msgf "%s" t);
    let s = s t in
    Cohttp_lwt_body.to_string body
    >>= Store.update s [id]
    >>= fun () -> add_to_reviewed s id
    >>= fun () -> S.respond ~headers ~status:`OK ~body:Cohttp_lwt_body.empty ()


  let update_review path s body =
    let id = List.hd path in
    let t = "update a review " ^ id in
    Log.info (fun msgf -> msgf "%s" t);
    let s = s t in
    Cohttp_lwt_body.to_string body
    >>= Store.update s [id]
    >>= S.respond ~headers ~status:`OK ~body:Cohttp_lwt_body.empty


  let read_review path s body =
    let id = List.hd path in
    let t = "read a review " ^ id in
    Log.info (fun msgf -> msgf "%s" t);
    let s = s t in
    match%lwt Store.read s [id] with
    | None -> respond_error `Not_found ("no review for " ^ id)
    | Some v ->
       Log.info (fun msgf -> msgf "%s" ("respond: " ^ v));
       let body = Cohttp_lwt_body.of_string v in
       S.respond ~headers ~status:`OK ~body ()


  let delete_review path s body =
    let id = List.hd path in
    let t = "delete a review " ^ id in
    Log.info (fun msgf -> msgf "%s" t);
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
    Log.info (fun msgf -> msgf "%s" t);
    let s = s t in
    match%lwt Store.read s [id] with
    | None -> respond_error `Not_found ("no review for " ^ id)
    | Some v ->
       meta_of_review v >>= fun meta ->
       Log.info (fun msgf -> msgf "%s" ("sending meta info:\n" ^ meta));
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
    Log.info (fun msgf -> msgf "connection %s from %s:%d"
               (Cohttp.Connection.to_string conn)
               (Ipaddr.V4.to_string ip) port);
    dispatch request s body

  let init_db s =
    let t = "init db with '/list' -> [] " in
    Log.info (fun msgf -> msgf "%s" t);    let s = s t in
    let v = Ezjsonm.to_string (`A []) in
    Store.update s ["list"]  v


  let reporter () =
    let report src lvl ~over k msgf =
      let name = Logs.Src.name src in
      let k _ = over (); k () in
      let ppf = Format.std_formatter in
      let pp_header ppf (l, h) =
        let l = Logs.(match l with
          | App -> "APP"
          | Error -> "ERR"
          | Warning -> "WARN"
          | Info -> "INFO"
          | Debug -> "DEBUG") in
        Format.fprintf ppf "[%s][%s]" h l in
      let with_stamp stamp fmt =
        let s = match stamp with
          | None -> 0. | Some span -> Mtime.to_s span in
        Format.kfprintf k ppf ("%a [%7.2fs] @[" ^^ fmt ^^ "@]@.")
          pp_header (lvl, name) s in
      msgf @@ fun ?header ?tags fmt ->
      match tags with
      | Some t when Logs.Tag.mem stamp t ->
         with_stamp (Logs.Tag.find stamp t) fmt
      | None ->
         Format.kfprintf k ppf ("%a @[" ^^ fmt ^^ "@]@.") pp_header (lvl, name) in
    { Logs.report = report }


  let start stack conf _clock () =
    Logs.set_level (Some Logs.Info);
    Logs.set_reporter (reporter ());
    Store.Repo.create config >>= fun repo ->
    Store.master task repo >>= fun s ->
    init_db s >>= fun () ->
    let http = S.make ~conn_closed:ignore ~callback:(handle_request s) () in
    Stack.listen_tcpv4 stack ~port:8443 (S.listen http);
    Stack.listen stack
end
