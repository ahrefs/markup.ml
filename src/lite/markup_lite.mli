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
type encoding = [ `Auto | `UTF_8 | `UTF_16BE | `UTF_16LE ]

module Token_tag : sig
  type t = {
    name : string;
    attributes : (string * string) list;
    self_closing : bool;
  }
end

type token =
  [ `Doctype of doctype
  | `Start of Token_tag.t
  | `End of Token_tag.t
  | `Char of int
  | `String of string
  | `Comment of string
  | `EOF ]

module Error = Markup_common.Error
module Ns = Markup_common.Ns

val signal_to_string : [< signal ] -> string

val parse_html :
  ?report:(location -> Error.t -> unit) ->
  ?encoding:encoding ->
  ?context:[ `Document | `Fragment of string ] ->
  ?depth_limit:int ->
  string ->
  (signal, sync) stream

val parse_tokens :
  ?report:(location -> Error.t -> unit) ->
  ?context:[ `Document | `Fragment of string ] ->
  ?depth_limit:int ->
  (location * token) list ->
  (signal, sync) stream

val iter : ('a -> unit) -> ('a, sync) stream -> unit

val write_html :
  ?escape_attribute:(Buffer.t -> string -> unit) ->
  ?escape_text:(Buffer.t -> string -> unit) ->
  Buffer.t ->
  (signal, sync) stream ->
  unit

val to_html_string :
  ?escape_attribute:(Buffer.t -> string -> unit) ->
  ?escape_text:(Buffer.t -> string -> unit) ->
  (signal, sync) stream ->
  string
