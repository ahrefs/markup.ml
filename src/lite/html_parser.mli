(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

val parse :
  ?depth_limit:int ->
  [< `Document | `Fragment of string ] ->
  Error.parse_handler ->
  Token_source.t ->
  (location * signal) Kstream.t
