(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

external unsafe_find : string -> int -> char -> int
  = "markup_lite_string_helpers_find"
[@@noalloc]

external unsafe_count : string -> int -> int -> char -> int
  = "markup_lite_string_helpers_count"
[@@noalloc]

let find string start character =
  if start < 0 || start > String.length string then
    invalid_arg "String_helpers.find";
  unsafe_find string start character

let count string start stop character =
  if start < 0 || stop < start || stop > String.length string then
    invalid_arg "String_helpers.count";
  unsafe_count string start stop character
