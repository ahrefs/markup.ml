(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

type location_out = { mutable line : int; mutable column : int }
type t

val create : string -> t
val location : unit -> location_out
val next : t -> Html_tokenizer.state -> location_out -> Html_tokenizer.token
val set_foreign : t -> (unit -> bool) -> unit
val push : t -> location * Html_tokenizer.token -> unit
