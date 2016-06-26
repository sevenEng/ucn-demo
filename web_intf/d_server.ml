#!/bin/env/ocaml

#use "topfind";;
#require "lwt";;
#require "astring";;
#require "ezjsonm";;
#require "cohttp.lwt";;

module Server = Cohttp_lwt_unix.Server

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


let make_server () =
  let callback conn req body =
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    let steps = Astring.String.cuts ~empty:false ~sep:"/" path in
    let headers = Cohttp.Header.init_with "Access-Control-Allow-Origin" "*" in
    match steps with
    | "title" :: tl ->
       let items = search_movie req body tl in
       let json = Ezjsonm.(list dict items) in
       let body =
         Ezjsonm.to_string json
         |> Cohttp_lwt_body.of_string in
       Server.respond ~headers ~status:`OK ~body ()
    | [] | ["index.html"] ->
       let fname = "index.html" in
       Printf.printf "respond file: %s\n%!" fname;
       Server.respond_file ~headers ~fname ()
    | _ ->
       let fname = Server.resolve_local_file ~docroot:"." ~uri in
       Printf.printf "try resolving local file: %s -> %s\n%!" path fname;
       Server.respond_file ~headers ~fname ()
  in
  let ctx = Cohttp_lwt_unix_net.init () in
  let port = 8080 in
  let mode = `TCP (`Port port) in
  let t = Server.make ~callback () in
  Printf.printf "listening on localhost:%d\n%!" port;
  Server.create ~ctx ~mode t


let () =
  make_server ()
  |> Lwt_main.run
