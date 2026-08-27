(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

type location_out = Markup__Html_tokenizer.location_out = {
  mutable line : int;
  mutable column : int;
}

type t

exception End_of_input

val create : Error.parse_handler -> string -> t
val of_tokens : (location * Html_tokenizer.token) list -> t
val location : unit -> location_out
val next : t -> Html_tokenizer.state -> location_out -> Html_tokenizer.token
val set_foreign : t -> (unit -> bool) -> unit
val push : t -> location * Html_tokenizer.token -> unit
