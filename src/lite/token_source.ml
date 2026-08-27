(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

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
  let length = String.length html in
  let byte index = Char.code (String.unsafe_get html index) in
  let rec continuations index count =
    if count = 0 then run index
    else if index < length && byte index land 0xC0 = 0x80 then
      continuations (index + 1) (count - 1)
    else false
  and run index =
    if index >= length then true
    else
      let first = byte index in
      if first < 0x80 then run (index + 1)
      else if first < 0xC2 then false
      else if first < 0xE0 then continuations (index + 1) 1
      else if first < 0xF0 then
        if index + 1 >= length then false
        else
          let second = byte (index + 1) in
          let low, high =
            if first = 0xE0 then (0xA0, 0xBF)
            else if first = 0xED then (0x80, 0x9F)
            else (0x80, 0xBF)
          in
          if second >= low && second <= high then continuations (index + 2) 1
          else false
      else if first < 0xF5 then
        if index + 1 >= length then false
        else
          let second = byte (index + 1) in
          let low, high =
            if first = 0xF0 then (0x90, 0xBF)
            else if first = 0xF4 then (0x80, 0x8F)
            else (0x80, 0xBF)
          in
          if second >= low && second <= high then continuations (index + 2) 2
          else false
      else false
  in
  run 0

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
