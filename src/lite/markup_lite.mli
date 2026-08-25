(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(** Small synchronous HTML parser using types shared with {!Markup}. *)

type async = Markup_common.async
type sync = Markup_common.sync
type ('data, 'sync) stream = ('data, 'sync) Markup_common.stream

type location = Markup_common.location
type name = Markup_common.name
type xml_declaration = Markup_common.xml_declaration = {
  version : string;
  encoding : string option;
  standalone : bool option;
}
type doctype = Markup_common.doctype = {
  doctype_name : string option;
  public_identifier : string option;
  system_identifier : string option;
  raw_text : string option;
  force_quirks : bool;
}
type signal = Markup_common.signal

module Error = Markup_common.Error
module Ns = Markup_common.Ns

val signal_to_string : [< signal ] -> string

val parse_html :
  ?report:(location -> Error.t -> unit) ->
  ?context:[ `Document | `Fragment of string ] ->
  string -> (signal, sync) stream

val iter : ('a -> unit) -> ('a, sync) stream -> unit
