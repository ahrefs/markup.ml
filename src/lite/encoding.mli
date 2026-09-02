(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

val decode_html : string -> string
(** Detect encoding, and decode to utf8 if necessary *)

val utf_16be : string -> string
val utf_16le : string -> string
