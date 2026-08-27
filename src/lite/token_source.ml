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
  mutable foreign : unit -> bool;
  mutable foreign_text : (string * int * int * int) option;
}

let valid_utf_8 = String.is_valid_utf_8

let replace_malformed html =
  let buffer = Buffer.create (String.length html + 16) in
  Uutf.String.fold_utf_8
    (fun () _ -> function
      | `Uchar uchar -> Uutf.Buffer.add_utf_8 buffer uchar
      | `Malformed _ -> Uutf.Buffer.add_utf_8 buffer Uutf.u_rep)
    () html;
  Buffer.contents buffer

let normalize_newlines html =
  if not (String.contains html '\r') then html
  else begin
    let length = String.length html in
    let buffer = Buffer.create length in
    let rec copy index =
      if index < length then
        match html.[index] with
        | '\r' ->
            Buffer.add_char buffer '\n';
            if index + 1 < length && html.[index + 1] = '\n' then
              copy (index + 2)
            else copy (index + 1)
        | c ->
            Buffer.add_char buffer c;
            copy (index + 1)
    in
    copy 0;
    Buffer.contents buffer
  end

(* The baseline UTF-8 decoder consumes one leading BOM, and its input
   preprocessor consumes one more leading U+FEFF. *)
let strip_leading_bom html =
  let length = String.length html in
  let has_bom index =
    index + 3 <= length
    && html.[index] = '\xEF'
    && html.[index + 1] = '\xBB'
    && html.[index + 2] = '\xBF'
  in
  let start = if not (has_bom 0) then 0 else if has_bom 3 then 6 else 3 in
  if start = 0 then html else String.sub html start (length - start)

let create html =
  let html = if valid_utf_8 html then html else replace_malformed html in
  let html = strip_leading_bom html in
  let html = normalize_newlines html in
  {
    scanner = Ragel_html_tokenizer.create html;
    pushed = [];
    foreign = (fun () -> false);
    foreign_text = None;
  }

let of_tokens tokens =
  let pushed =
    List.map (fun ((line, column), token) -> { token; line; column }) tokens
  in
  {
    scanner = Ragel_html_tokenizer.create "";
    pushed;
    foreign = (fun () -> false);
    foreign_text = None;
  }

let location () = { line = 1; column = -1 }

let next_foreign_character source (out : location_out) text index line column =
  let decoded = String.get_utf_8_uchar text index in
  let uchar = Uchar.utf_decode_uchar decoded in
  let next = index + Uchar.utf_decode_length decoded in
  source.foreign_text <-
    (if next < String.length text then Some (text, next, line, column) else None);
  out.line <- line;
  out.column <- column;
  `Char (Uchar.to_int uchar)

let next source state (out : location_out) =
  match source.pushed with
  | { token; line; column } :: rest ->
      source.pushed <- rest;
      out.line <- line;
      out.column <- column;
      token
  | [] ->
      begin match source.foreign_text with
      | Some (text, index, line, column) ->
          next_foreign_character source out text index line column
      | None ->
          let foreign = source.foreign () in
          begin match
            Ragel_html_tokenizer.next source.scanner state foreign out
          with
          | `String text when foreign && text <> "" ->
              next_foreign_character source out text 0 out.line out.column
          | token -> token
          end
      end

let set_foreign source foreign = source.foreign <- foreign

let push source ((line, column), token) =
  source.pushed <- { token; line; column } :: source.pushed
