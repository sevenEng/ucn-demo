open Lwt

module Server = Cohttp_lwt_unix.Server
module Client = Cohttp_lwt_unix.Client

let headers = Cohttp.Header.init_with "Access-Control-Allow-Origin" "*"

let ip = "10.0.0.1"
let port = 8080

let gk_ip = "10.0.0.254"
let bridge_ip = "10.0.0.2"

let gk_port = 8080
let review_port = 10011
let catalog_port = 10012


module type ENDPOINT = sig
    val host :  string
    val port : int
end

module GateKeeper = struct

    let dispatch req body tl =
      let path = String.concat "/" tl in
      let () = Printf.printf "GateKeeper: %s\n%!" path in
      let uri = Uri.make ~scheme:"http" ~host:gk_ip ~port:gk_port ~path () in
      match Cohttp.Request.meth req with
      | `GET ->
         Client.get uri
      | `POST ->
         Client.post ~body uri
      | _ as m ->
         let m = Cohttp.Code.string_of_method m in
         Lwt.fail_with ("[GK] unsupported http metohd: " ^ m)
end

module Review = struct

    let dispatch req body tl =
      let path = String.concat "/" tl in
      let () = Printf.printf "Review: %s\n%!" path in
      let uri = Uri.make ~scheme:"https" ~host:bridge_ip ~port:review_port ~path () in
      match Cohttp.Request.meth req with
      | `GET ->
         Client.get uri
      | `POST ->
         Client.post ~body uri
      | _ as m ->
         let m = Cohttp.Code.string_of_method m in
         Lwt.fail_with ("[Review] unsupported http metohd: " ^ m)
end


module Catalog = struct

    let dispatch req body tl =
      let path = String.concat "/" tl in
      let () = Printf.printf "Catalog: %s\n%!" path in
      let uri = Uri.make ~scheme:"https" ~host:bridge_ip ~port:catalog_port ~path () in
      match Cohttp.Request.meth req with
      | `GET ->
         Client.get uri
      | `POST ->
         Client.post ~body uri
      | _ as m ->
         let m = Cohttp.Code.string_of_method m in
         Lwt.fail_with ("[Catalog] unsupported http metohd: " ^ m)
end


let search_movie req body tl =
  let title = List.hd tl in
  Printf.printf "search request: %s\n%!" title;
  let lim = 5 in
  let rec item acc cnt =
    if cnt < lim then
      let title =
        Filename.temp_file title "_fake"
        |> Filename.basename in
      let n = ["id", `String (string_of_int cnt); "title", `String title] in
      item (n :: acc) (cnt + 1)
    else acc in
  item [] 0


let make_server conf =
  let callback conn req body =
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    let steps = Astring.String.cuts ~empty:false ~sep:"/" path in
    match steps with
    | "title" :: tl ->
       let items = search_movie req body tl in
       let json = Ezjsonm.(list dict items) in
       let body =
         Ezjsonm.to_string json
         |> Cohttp_lwt_body.of_string in
       Server.respond ~headers ~status:`OK ~body ()
    | "gatekeeper" :: tl ->
       GateKeeper.dispatch req body tl >>= fun (res, body) ->
       let status = Cohttp.Response.status res in
       let hdr_lst =
         Cohttp.Response.headers res
         |> Cohttp.Header.to_list in
       let headers = Cohttp.Header.add_list headers hdr_lst in
       Server.respond ~headers ~status ~body ()
    | "review" :: tl ->
       Review.dispatch req body tl >>= fun (res, body) ->
       let status = Cohttp.Response.status res in
       let hdr_lst =
         Cohttp.Response.headers res
         |> Cohttp.Header.to_list in
       let headers = Cohttp.Header.add_list headers hdr_lst in
       Server.respond ~headers ~status ~body ()
    | "catalog" :: tl ->
       Catalog.dispatch req body tl >>= fun (res, body) ->
       let status = Cohttp.Response.status res in
       let hdr_lst =
         Cohttp.Response.headers res
         |> Cohttp.Header.to_list in
       let headers = Cohttp.Header.add_list headers hdr_lst in
       Server.respond ~headers ~status ~body ()
    | [] | ["index.html"] ->
       let fname = "index.html" in
       Printf.printf "respond file: %s\n%!" fname;
       Server.respond_file ~headers ~fname ()
    | _ ->
       let fname = Server.resolve_local_file ~docroot:"." ~uri in
       Printf.printf "try resolving local file: %s -> %s\n%!" path fname;
       Server.respond_file ~headers ~fname ()
  in

  Conduit_lwt_unix.init ~src:ip () >>= fun conduit_ctx ->
  let ctx = Cohttp_lwt_unix_net.init ~ctx:conduit_ctx () in
  let mode = `TCP (`Port port) in
  let t = Server.make ~callback () in
  Printf.printf "listening on %s:%d\n%!" ip port;
  Server.create ~ctx ~mode t


let () = Lwt_main.run (make_server ())
