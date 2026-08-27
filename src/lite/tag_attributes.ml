(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(* Port of the attribute tokenizer states of src/baseline/html_tokenizer.ml
   (8.2.4.34-43 plus the self-closing state). [scan] is given the index of
   the first byte after a start tag's name and returns the attributes in
   source order with raw (undecoded) values, whether the tag is
   self-closing, and the index after the closing '>'. [ok] is false when
   the input ends inside the tag, in which case no token is emitted. *)

type result = {
  attributes : (string * string) list;
  self_closing : bool;
  next : int;
  ok : bool;
}

let u_rep_utf_8 = "\xEF\xBF\xBD"

let add buffer byte =
  if byte = '\x00' then Buffer.add_string buffer u_rep_utf_8
  else Buffer.add_char buffer byte

let add_lowercase buffer byte =
  if byte = '\x00' then Buffer.add_string buffer u_rep_utf_8
  else Buffer.add_char buffer (Char.lowercase_ascii byte)

let is_whitespace = function '\t' | '\n' | '\x0C' | ' ' -> true | _ -> false

let scan data start =
  let length = String.length data in
  let attributes = ref [] in
  let name = Buffer.create 16 in
  let value = Buffer.create 32 in
  let commit () =
    if Buffer.length name > 0 then
      attributes := (Buffer.contents name, Buffer.contents value) :: !attributes;
    Buffer.clear name;
    Buffer.clear value
  in
  let finish ?(self_closing = false) index =
    commit ();
    { attributes = List.rev !attributes; self_closing; next = index; ok = true }
  in
  let eof index =
    { attributes = []; self_closing = false; next = index; ok = false }
  in
  let rec before_name index =
    if index >= length then eof index
    else
      let byte = data.[index] in
      if is_whitespace byte then before_name (index + 1)
      else if byte = '/' then self_closing_start (index + 1)
      else if byte = '>' then finish (index + 1)
      else begin
        add_lowercase name byte;
        in_name (index + 1)
      end
  and in_name index =
    if index >= length then eof index
    else
      let byte = data.[index] in
      if is_whitespace byte then after_name (index + 1)
      else if byte = '/' then begin
        commit ();
        self_closing_start (index + 1)
      end
      else if byte = '=' then before_value (index + 1)
      else if byte = '>' then finish (index + 1)
      else begin
        add_lowercase name byte;
        in_name (index + 1)
      end
  and after_name index =
    if index >= length then eof index
    else
      let byte = data.[index] in
      if is_whitespace byte then after_name (index + 1)
      else if byte = '/' then begin
        commit ();
        self_closing_start (index + 1)
      end
      else if byte = '=' then before_value (index + 1)
      else if byte = '>' then finish (index + 1)
      else begin
        commit ();
        add_lowercase name byte;
        in_name (index + 1)
      end
  and before_value index =
    if index >= length then eof index
    else
      let byte = data.[index] in
      if is_whitespace byte then before_value (index + 1)
      else if byte = '"' || byte = '\'' then quoted byte (index + 1)
      else if byte = '>' then finish (index + 1)
      else begin
        add value byte;
        unquoted (index + 1)
      end
  and quoted quote index =
    if index >= length then eof index
    else
      let byte = data.[index] in
      if byte = quote then begin
        commit ();
        after_quoted (index + 1)
      end
      else begin
        add value byte;
        quoted quote (index + 1)
      end
  and unquoted index =
    if index >= length then eof index
    else
      let byte = data.[index] in
      if is_whitespace byte then begin
        commit ();
        before_name (index + 1)
      end
      else if byte = '>' then finish (index + 1)
      else begin
        add value byte;
        unquoted (index + 1)
      end
  and after_quoted index =
    if index >= length then eof index
    else
      let byte = data.[index] in
      if is_whitespace byte then before_name (index + 1)
      else if byte = '/' then self_closing_start (index + 1)
      else if byte = '>' then finish (index + 1)
      else before_name index
  and self_closing_start index =
    if index >= length then eof index
    else if data.[index] = '>' then finish ~self_closing:true (index + 1)
    else before_name index
  in
  before_name start
