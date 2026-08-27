(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

type location_out = Ragel_html_tokenizer.location_out = {
  mutable line : int;
  mutable column : int;
}

type pushed_token = { token : Html_tokenizer.token; line : int; column : int }

type t = {
  scanner : Ragel_html_tokenizer.t;
  mutable pushed : pushed_token list;
}

let valid_utf_8 html =
  try
    Uutf.String.fold_utf_8
      (fun () _ -> function `Uchar _ -> () | `Malformed _ -> raise Exit)
      () html;
    true
  with Exit -> false

let replace_malformed html =
  let buffer = Buffer.create (String.length html + 16) in
  Uutf.String.fold_utf_8
    (fun () _ -> function
      | `Uchar uchar -> Uutf.Buffer.add_utf_8 buffer uchar
      | `Malformed _ -> Uutf.Buffer.add_utf_8 buffer Uutf.u_rep)
    () html;
  Buffer.contents buffer

let create html =
  let html = if valid_utf_8 html then html else replace_malformed html in
  { scanner = Ragel_html_tokenizer.create html; pushed = [] }

let location () = { line = 1; column = -1 }

let next source state (out : location_out) =
  match source.pushed with
  | { token; line; column } :: rest ->
      source.pushed <- rest;
      out.line <- line;
      out.column <- column;
      token
  | [] -> Ragel_html_tokenizer.next source.scanner state out

let push source ((line, column), token) =
  source.pushed <- { token; line; column } :: source.pushed
