module type LOG = sig
  val app   : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
  val err   : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
  val warn  : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
  val info  : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
  val debug : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
end

let stamp : Mtime.span Logs.Tag.def = Logs.Tag.def "stamp.tag" Mtime.pp_span

let src_stamp_log src =
  let src = Logs.Src.create src in
  let module L = struct
    let counter = Mtime.counter ()
    let stamp_tag () =
      Logs.Tag.add stamp (Mtime.count counter) Logs.Tag.empty
    let log l f =
      Logs.msg ~src l (fun m -> f (m ~tags:(stamp_tag ()) ~header:""))

    let app f = log Logs.App f
    let err f = log Logs.Error f
    let warn f = log Logs.Warning f
    let info f = log Logs.Info f
    let debug f = log Logs.Debug f
    end in
  (module L : LOG)

let src_stamp_reporter () =
  let report src lvl ~over k msgf =
    let src = Logs.Src.name src in
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
                      pp_header (lvl, src) s in
    msgf @@ fun ?header ?tags fmt ->
      match tags with
      | Some t when Logs.Tag.mem stamp t ->
         with_stamp (Logs.Tag.find stamp t) fmt
      | None ->
         Format.kfprintf k ppf ("%a @[" ^^ fmt ^^ "@]@.") pp_header (lvl, src) in
  { Logs.report = report }
