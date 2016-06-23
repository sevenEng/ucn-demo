(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Mirage

let ipv4_conf =
  let i = Ipaddr.V4.of_string_exn in
  {
    address  = i "10.0.0.2";
    netmask  = i "255.255.255.0";
    gateways = [i "10.0.0.1"];
  }

(*
let direct_stack_impl console () =
  let open Key in
  if_impl (value @@ dhcp ())
    (direct_stackv4_with_dhcp console tap0)
    (direct_stackv4_with_static_ipv4 console tap0 ipv4_conf)
*)
(*
let stack =
  let open Key in
  if_impl is_xen
    (generic_stackv4 default_console tap0)
    (socket_stackv4 default_console [Ipaddr.V4.any])
*)

let stack =
  let open Key in
  if_impl is_xen
          (direct_stackv4_with_static_ipv4 default_console tap0 ipv4_conf)
          (socket_stackv4 default_console [Ipaddr.V4.any])

let main =
  let libraries = [
    "logs";
    "mirage-http";
    "mirage-logs";
    "git";
    "irmin.mirage"] in
  foreign ~libraries ~deps:[abstract nocrypto] "Unikernel.Main" (stackv4 @-> clock @-> job)

let () =
  register "review" [
    main $ stack $ default_clock
  ]
