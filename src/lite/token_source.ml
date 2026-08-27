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

(* Like src/baseline: the uutf decoder drops a leading BOM, then the input
   preprocessor drops the first U+FEFF found anywhere in the stream. *)
let remove_first_bom html =
  let length = String.length html in
  let rec find index =
    if index + 3 > length then None
    else if
      html.[index] = '\xEF'
      && html.[index + 1] = '\xBB'
      && html.[index + 2] = '\xBF'
    then Some index
    else find (index + 1)
  in
  match find 0 with
  | None -> html
  | Some index ->
      String.sub html 0 index ^ String.sub html (index + 3) (length - index - 3)

let strip_leading_bom html =
  let bom = "\xEF\xBB\xBF" in
  if String.length html >= 3 && String.sub html 0 3 = bom then
    remove_first_bom (String.sub html 3 (String.length html - 3))
  else remove_first_bom html

let create html =
  let html = if valid_utf_8 html then html else replace_malformed html in
  let html = strip_leading_bom html in
  let html = normalize_newlines html in
  { scanner = Ragel_html_tokenizer.create html; pushed = [] }

let of_tokens tokens =
  let pushed =
    List.map (fun ((line, column), token) -> { token; line; column }) tokens
  in
  { scanner = Ragel_html_tokenizer.create ""; pushed }

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
