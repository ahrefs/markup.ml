(* Derived from Devkit's htmlStream_ragel.ml.rl.
   Devkit is distributed under LGPL-2.1-only with the OCaml linking exception. *)

type location_out = { mutable line : int; mutable column : int }
type t

val create : string -> t

val next :
  t ->
  Html_tokenizer.state ->
  bool ->
  drop_candidate:bool ->
  location_out ->
  Html_tokenizer.token
