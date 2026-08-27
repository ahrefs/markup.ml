type state = Markup__Html_tokenizer.state
type command = Next | Set_state of state | Set_foreign of bool

type tag = {
  name : string;
  attributes : (string * string) list;
  self_closing : bool;
}

type token =
  [ `Doctype of Markup_common.doctype
  | `Start of tag
  | `End of tag
  | `Char of int
  | `String of string
  | `Comment of string
  | `EOF ]

type termination = [ `EOF | `Exception of string | `Timeout ]

type outcome = {
  tokens : (Markup_common.location * token) list;
  reports : (Markup_common.location * Markup_common.Error.t) list;
  termination : termination;
}

val script :
  eof_location:Markup_common.location ->
  (Markup_common.location * int) list ->
  command list ->
  outcome * outcome

val compare_data :
  eof_location:Markup_common.location ->
  (Markup_common.location * int) list ->
  outcome * outcome

val shadow_parse :
  ?depth_limit:int ->
  [< `Document | `Fragment of string ] option ->
  eof_location:Markup_common.location ->
  (Markup_common.location * int) list ->
  termination * (Markup_common.location * Markup_common.Error.t) list
