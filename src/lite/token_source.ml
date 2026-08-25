(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

type location_out = Ragel_html_tokenizer.location_out = {
  mutable line : int;
  mutable column : int;
}

type pushed_token = {
  token : Html_tokenizer.token;
  line : int;
  column : int;
}

type t = {
  scanner : Ragel_html_tokenizer.t;
  mutable pushed : pushed_token list;
}

let create html =
  {scanner = Ragel_html_tokenizer.create html; pushed = []}

let location () = {line = 1; column = -1}

let next source state (out : location_out) =
  match source.pushed with
  | {token; line; column}::rest ->
    source.pushed <- rest;
    out.line <- line;
    out.column <- column;
    token
  | [] -> Ragel_html_tokenizer.next source.scanner state out

let push source ((line, column), token) =
  source.pushed <- {token; line; column}::source.pushed
