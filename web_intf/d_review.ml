module Review = struct

  module Client = Cohttp_lwt_unix.Client

  let host = Uri.of_string "https://localhost:4433/"

  let dispatch tl req body =
    let path = String.concat "/" tl in
    let uri = Uri.with_path host path in
    match Cohttp.Request.meth req with
    | `GET -> Client.get uri
    | `POST -> Client.post ~body uri
    | _ as m ->
       let meth = Cohttp.Code.string_of_method m in
       Lwt.fail_with ("not know request meth: " ^ meth)
end
