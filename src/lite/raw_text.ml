(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(* Port of the RCDATA, RAWTEXT, and script data tokenizer states of
   src/baseline/html_tokenizer.ml (8.2.4.3, 8.2.4.5, 8.2.4.6, and
   8.2.4.11-8.2.4.43), operating on the normalized input string. [scan] is
   given the index just after the '>' of the opening tag and the lowercase
   element name, and consumes the element's raw content up to and including
   its end tag, if any. *)

type result = { text : string; had_end_tag : bool; next : int }

let u_rep_utf_8 = "\xEF\xBF\xBD"

let add buffer byte =
  if byte = '\x00' then Buffer.add_string buffer u_rep_utf_8
  else Buffer.add_char buffer byte

let is_whitespace = function '\t' | '\n' | '\x0C' | ' ' -> true | _ -> false
let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let plaintext data start =
  let length = String.length data in
  let buffer = Buffer.create (length - start) in
  for index = start to length - 1 do
    add buffer data.[index]
  done;
  { text = Buffer.contents buffer; had_end_tag = false; next = length }

let scan ?(drop_end_tag_candidate = false) data start tag =
  let length = String.length data in
  let decode = match tag with "title" | "textarea" -> true | _ -> false in
  let buffer = Buffer.create 256 in
  let finish had_end_tag next =
    let text = Buffer.contents buffer in
    let text = if decode then Html_entity_decoder.decode text else text in
    { text; had_end_tag; next }
  in
  let word_is first after word =
    after - first = String.length word
    && begin
      let rec check offset =
        offset >= String.length word
        || Char.lowercase_ascii data.[first + offset] = word.[offset]
           && check (offset + 1)
      in
      check 0
    end
  in
  (* End tag attributes are parsed only to find the terminating '>'; names
     and values are discarded. *)
  let rec before_attribute_name index =
    if index >= length then None
    else
      match data.[index] with
      | byte when is_whitespace byte -> before_attribute_name (index + 1)
      | '/' -> self_closing_start_tag (index + 1)
      | '>' -> Some (index + 1)
      | _ -> attribute_name (index + 1)
  and attribute_name index =
    if index >= length then None
    else
      match data.[index] with
      | byte when is_whitespace byte -> after_attribute_name (index + 1)
      | '/' -> self_closing_start_tag (index + 1)
      | '=' -> before_attribute_value (index + 1)
      | '>' -> Some (index + 1)
      | _ -> attribute_name (index + 1)
  and after_attribute_name index =
    if index >= length then None
    else
      match data.[index] with
      | byte when is_whitespace byte -> after_attribute_name (index + 1)
      | '/' -> self_closing_start_tag (index + 1)
      | '=' -> before_attribute_value (index + 1)
      | '>' -> Some (index + 1)
      | _ -> attribute_name (index + 1)
  and before_attribute_value index =
    if index >= length then None
    else
      match data.[index] with
      | byte when is_whitespace byte -> before_attribute_value (index + 1)
      | ('"' | '\'') as quote -> attribute_value_quoted quote (index + 1)
      | '>' -> Some (index + 1)
      | _ -> attribute_value_unquoted (index + 1)
  and attribute_value_quoted quote index =
    if index >= length then None
    else if data.[index] = quote then after_attribute_value_quoted (index + 1)
    else attribute_value_quoted quote (index + 1)
  and after_attribute_value_quoted index =
    if index >= length then None
    else
      match data.[index] with
      | byte when is_whitespace byte -> before_attribute_name (index + 1)
      | '/' -> self_closing_start_tag (index + 1)
      | '>' -> Some (index + 1)
      | _ -> before_attribute_name index
  and attribute_value_unquoted index =
    if index >= length then None
    else
      match data.[index] with
      | byte when is_whitespace byte -> before_attribute_name (index + 1)
      | '>' -> Some (index + 1)
      | _ -> attribute_value_unquoted (index + 1)
  and self_closing_start_tag index =
    if index >= length then None
    else if data.[index] = '>' then Some (index + 1)
    else before_attribute_name index
  in
  let finish_tag = function
    | Some next -> finish true next
    | None -> finish false length
  in
  let rec text index =
    if index >= length then finish false index
    else
      match data.[index] with
      | '<' -> text_less_than_sign index (index + 1)
      | byte ->
          add buffer byte;
          text (index + 1)
  and text_less_than_sign lt index =
    if index < length && data.[index] = '/' then end_tag_open text lt (index + 1)
    else begin
      Buffer.add_char buffer '<';
      text index
    end
  and end_tag_open state lt index =
    if index < length && is_letter data.[index] then
      end_tag_name state lt (index + 1)
    else begin
      Buffer.add_string buffer
        (if index >= length && drop_end_tag_candidate then "<" else "</");
      state index
    end
  and end_tag_name state lt index =
    let appropriate () = word_is (lt + 2) index tag in
    let dump () =
      if drop_end_tag_candidate then Buffer.add_char buffer '<'
      else Buffer.add_substring buffer data lt (index - lt);
      state index
    in
    if index >= length then dump ()
    else
      match data.[index] with
      | byte when is_whitespace byte && appropriate () ->
          finish_tag (before_attribute_name (index + 1))
      | '/' when appropriate () ->
          finish_tag (self_closing_start_tag (index + 1))
      | '>' when appropriate () -> finish true (index + 1)
      | byte when is_letter byte -> end_tag_name state lt (index + 1)
      | _ -> dump ()
  and script index =
    if index >= length then finish false index
    else
      match data.[index] with
      | '<' -> script_less_than_sign index (index + 1)
      | byte ->
          add buffer byte;
          script (index + 1)
  and script_less_than_sign lt index =
    if index >= length then begin
      Buffer.add_char buffer '<';
      script index
    end
    else
      match data.[index] with
      | '/' -> end_tag_open script lt (index + 1)
      | '!' ->
          Buffer.add_string buffer "<!";
          escape_start (index + 1)
      | _ ->
          Buffer.add_char buffer '<';
          script index
  and escape_start index =
    if index < length && data.[index] = '-' then begin
      Buffer.add_char buffer '-';
      escape_start_dash (index + 1)
    end
    else script index
  and escape_start_dash index =
    if index < length && data.[index] = '-' then begin
      Buffer.add_char buffer '-';
      escaped_dash_dash (index + 1)
    end
    else script index
  and escaped index =
    if index >= length then finish false index
    else
      match data.[index] with
      | '-' ->
          Buffer.add_char buffer '-';
          escaped_dash (index + 1)
      | '<' -> escaped_less_than_sign index (index + 1)
      | byte ->
          add buffer byte;
          escaped (index + 1)
  and escaped_dash index =
    if index >= length then finish false index
    else
      match data.[index] with
      | '-' ->
          Buffer.add_char buffer '-';
          escaped_dash_dash (index + 1)
      | '<' -> escaped_less_than_sign index (index + 1)
      | byte ->
          add buffer byte;
          escaped (index + 1)
  and escaped_dash_dash index =
    if index >= length then finish false index
    else
      match data.[index] with
      | '-' ->
          Buffer.add_char buffer '-';
          escaped_dash_dash (index + 1)
      | '<' -> escaped_less_than_sign index (index + 1)
      | '>' ->
          Buffer.add_char buffer '>';
          script (index + 1)
      | byte ->
          add buffer byte;
          escaped (index + 1)
  and escaped_less_than_sign lt index =
    if index >= length then begin
      Buffer.add_char buffer '<';
      escaped index
    end
    else
      match data.[index] with
      | '/' -> end_tag_open escaped lt (index + 1)
      | byte when is_letter byte ->
          Buffer.add_char buffer '<';
          Buffer.add_char buffer byte;
          double_escape_start index (index + 1)
      | _ ->
          Buffer.add_char buffer '<';
          escaped index
  and double_escape_start first index =
    if index >= length then escaped index
    else
      match data.[index] with
      | ('\t' | '\n' | '\x0C' | ' ' | '/' | '>') as byte ->
          Buffer.add_char buffer byte;
          if word_is first index "script" then double_escaped (index + 1)
          else escaped (index + 1)
      | byte when is_letter byte ->
          Buffer.add_char buffer byte;
          double_escape_start first (index + 1)
      | _ -> escaped index
  and double_escaped index =
    if index >= length then finish false index
    else
      match data.[index] with
      | '-' ->
          Buffer.add_char buffer '-';
          double_escaped_dash (index + 1)
      | '<' ->
          Buffer.add_char buffer '<';
          double_escaped_less_than_sign (index + 1)
      | byte ->
          add buffer byte;
          double_escaped (index + 1)
  and double_escaped_dash index =
    if index >= length then finish false index
    else
      match data.[index] with
      | '-' ->
          Buffer.add_char buffer '-';
          double_escaped_dash_dash (index + 1)
      | '<' ->
          Buffer.add_char buffer '<';
          double_escaped_less_than_sign (index + 1)
      | byte ->
          add buffer byte;
          double_escaped (index + 1)
  and double_escaped_dash_dash index =
    if index >= length then finish false index
    else
      match data.[index] with
      | '-' ->
          Buffer.add_char buffer '-';
          double_escaped_dash_dash (index + 1)
      | '<' ->
          Buffer.add_char buffer '<';
          double_escaped_less_than_sign (index + 1)
      | '>' ->
          Buffer.add_char buffer '>';
          script (index + 1)
      | byte ->
          add buffer byte;
          double_escaped (index + 1)
  and double_escaped_less_than_sign index =
    if index < length && data.[index] = '/' then begin
      Buffer.add_char buffer '/';
      double_escape_end (index + 1) (index + 1)
    end
    else double_escaped index
  and double_escape_end first index =
    if index >= length then double_escaped index
    else
      match data.[index] with
      | ('\t' | '\n' | '\x0C' | ' ' | '/' | '>') as byte ->
          Buffer.add_char buffer byte;
          if word_is first index "script" then escaped (index + 1)
          else double_escaped (index + 1)
      | byte when is_letter byte ->
          Buffer.add_char buffer byte;
          double_escape_end first (index + 1)
      | _ -> double_escaped index
  in
  match tag with "script" -> script start | _ -> text start
