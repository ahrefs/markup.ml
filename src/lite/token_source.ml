(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

type location_out = {
  mutable line : int;
  mutable column : int;
}

type pushed_token = {
  token : Html_tokenizer.token;
  line : int;
  column : int;
}

type t = {
  mutable tokens : (location * Html_tokenizer.token) list;
  mutable pushed : pushed_token list;
  mutable eof_line : int;
  mutable eof_column : int;
}

let create html =
  {tokens = Ragel_html_tokenizer.tokenize html;
   pushed = [];
   eof_line = 1;
   eof_column = -1}

let location () = {line = 1; column = -1}

let set_location (out : location_out) line column =
  out.line <- line;
  out.column <- column

let next source (_state : Html_tokenizer.state) out =
  match source.pushed with
  | {token; line; column}::rest ->
    source.pushed <- rest;
    set_location out line column;
    token
  | [] ->
    match source.tokens with
    | ((line, column), token)::rest ->
      source.tokens <- rest;
      set_location out line column;
      begin match token with
      | `EOF ->
        source.eof_line <- line;
        source.eof_column <- column
      | _ -> ()
      end;
      token
    | [] ->
      set_location out source.eof_line source.eof_column;
      `EOF

let push source ((line, column), token) =
  source.pushed <- {token; line; column}::source.pushed
