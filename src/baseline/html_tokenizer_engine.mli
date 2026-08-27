open Common

type state = Html_tokenizer_core.state
type token = Html_tokenizer_core.token

type input = Scalar of location * int | End

type action =
  | Await_input
  | Emit of location * token
  | Report of location * Error.t
  | Finished
  | Failed of exn

type t

val create : (unit -> location) -> t
val drive : t -> action
val supply : t -> input -> unit
val resume_report : t -> unit
val fail_report : t -> exn -> unit
val set_state : t -> state -> unit
val set_foreign : t -> (unit -> bool) -> unit
