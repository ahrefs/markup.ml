(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

type t =
  [ `Decoding_error of string * string
  | `Bad_token of string * string * string
  | `Unexpected_eoi of string
  | `Bad_document of string
  | `Unmatched_start_tag of string
  | `Unmatched_end_tag of string
  | `Bad_namespace of string
  | `Misnested_tag of string * string * (string * string) list
  | `Bad_content of string ]

val to_string : ?location:int * int -> t -> string
