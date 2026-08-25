(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(** Types shared by the [markup] and [markup.tiny] libraries.

    Both libraries alias the types below, so that streams produced by one are
    accepted directly by consumers written against the other. Nothing here is
    intended to be used directly by applications; use [Markup] or
    [Markup_tiny]. *)

(** Internal stream implementation, exposed so that libraries built on top of
    this one can construct the shared stream type. Not a stable interface. *)
module Kstream = Kstream
module Stream = Stream

module Error = Error

(** {2 Streams} *)

type async
type sync
(** Phantom types for use with [('a, 's) stream] in place of ['s]. They are
    distinct and abstract, so that a [sync] stream cannot be passed where an
    [async] stream is expected, and vice versa. *)

type ('a, 's) stream = ('a, 's) Stream.t
(** Streams of elements of type ['a]. The operations on streams are
    {!Kstream}'s; see {!Stream.Private} for converting between a [Kstream.t]
    and a [stream] at no cost. *)

(** {2 Errors} *)

type location = int * int

val compare_locations : location -> location -> int

(** {2 Signals} *)

type name = string * string

type xml_declaration = {
  version : string;
  encoding : string option;
  standalone : bool option;
}

type doctype = {
  doctype_name : string option;
  public_identifier : string option;
  system_identifier : string option;
  raw_text : string option;
  force_quirks : bool;
}

type signal =
  [ `Start_element of name * (name * string) list
  | `End_element
  | `Text of string list
  | `Xml of xml_declaration
  | `Doctype of doctype
  | `PI of string * string
  | `Comment of string ]

val signal_to_string : [< signal ] -> string

(** Common namespace URIs. *)
module Ns : sig
  val html : string
  val svg : string
  val mathml : string
  val xml : string
  val xmlns : string
  val xlink : string
end
