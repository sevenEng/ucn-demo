open Mirage

let ipv4_conf =
  let i = Ipaddr.V4.of_string_exn in
  {
    address  = i "10.0.0.2";
    netmask  = i "255.255.255.0";
    gateways = [i "10.0.0.1"];
  }

let stack =
  let open Key in
  if_impl is_xen
          (direct_stackv4_with_static_ipv4 default_console tap0 ipv4_conf)
          (socket_stackv4 default_console [Ipaddr.V4.any])

let main =
  let libraries = ["irmin.git";
                   "mirage-http"; "irmin.mirage";
                   "conduit.mirage";
                   "cohttp.lwt-core"; "ezjsonm"; "lwt"; "lwt.ppx";
                   "logs"; "mirage-logs"; "astring"] in
  let packages = [
      "irmin";
      "mirage-http";
      "mirage-flow";
      "mirage-types-lwt";
      "channel";
      "git";
      "mirage-git";
      "cohttp";
      "ezjsonm";
      "lwt";
      "logs";
      "astring"] in
  foreign ~libraries ~packages ~deps:[abstract nocrypto]
    "Unikernel.Main" (stackv4 @-> clock @-> job)

let () =
  register "ucn-catalog" [
    main $ stack $ default_clock
  ]
