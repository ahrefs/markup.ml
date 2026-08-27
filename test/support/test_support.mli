(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Markup__Common

module Ragel_html_tokenizer : sig
  val tokenize :
    string -> (Markup__Common.location * Markup__Html_tokenizer.token) list
end

module Ragel_token_snapshot : sig
  type t = {
    source : string;
    input : string option;
    tokens : (Markup__Common.location * Markup__Html_tokenizer.token) list;
  }
end

val wrong_k : string -> _ cont
val with_text_limit : int -> (unit -> unit) -> unit

val expect_error :
  ?allow_recovery:int ->
  location ->
  Markup.Error.t ->
  (Markup__Error.parse_handler -> unit) ->
  unit

type 'a general_signal = S of 'a | E of Markup.Error.t

val expect_signals :
  ?prefix:bool ->
  ('a -> string) ->
  string ->
  (int * int * 'a general_signal) list ->
  Markup__Error.parse_handler * (location * 'a -> unit cps) * (unit -> unit)

val expect_no_location_signals :
  ?prefix:bool ->
  (([> `Comment of 'b | `Doctype of 'c ] as 'a) -> string) ->
  string ->
  (int * int * 'a general_signal) list ->
  (int * int -> Markup__Error.t -> (exn -> unit) -> (unit -> 'd) -> 'd)
  * ((int * int) * 'a -> (exn -> unit) -> (unit -> 'e) -> 'e)
  * (unit -> unit)

val expect_strings :
  string ->
  string general_signal list ->
  Markup__Error.write_handler * (string -> unit cps) * (unit -> unit)

val iter : ('a -> unit cps) -> 'a Markup__Kstream.t -> unit
