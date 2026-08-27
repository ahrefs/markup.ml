(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(* Port of the comment, bogus comment, and doctype tokenizer states of
   src/baseline/html_tokenizer.ml, operating on the normalized input string.
   [scan] is given the index of the '!' or '?' that follows '<' and returns
   the token together with the index of the first unconsumed byte. *)

open Common

type result = { token : Html_tokenizer.token; next : int }

let u_rep_utf_8 = "\xEF\xBF\xBD"

let add buffer byte =
  if byte = '\x00' then Buffer.add_string buffer u_rep_utf_8
  else Buffer.add_char buffer byte

let is_whitespace = function '\t' | '\n' | '\x0C' | ' ' -> true | _ -> false

let matches_lowercase data index keyword =
  index + String.length keyword <= String.length data
  && begin
    let rec check offset =
      offset >= String.length keyword
      || Char.lowercase_ascii data.[index + offset] = keyword.[offset]
         && check (offset + 1)
    in
    check 0
  end

let bogus_comment data start =
  let length = String.length data in
  let buffer = Buffer.create 32 in
  let rec consume index =
    if index >= length then
      { token = Html_tokenizer.Comment (Buffer.contents buffer); next = index }
    else
      match data.[index] with
      | '>' ->
          {
            token = Html_tokenizer.Comment (Buffer.contents buffer);
            next = index + 1;
          }
      | byte ->
          add buffer byte;
          consume (index + 1)
  in
  consume start

let comment data start =
  let length = String.length data in
  let buffer = Buffer.create 64 in
  let finish index =
    { token = Html_tokenizer.Comment (Buffer.contents buffer); next = index }
  in
  let rec comment_start index =
    if index >= length then finish index
    else
      match data.[index] with
      | '-' -> comment_start_dash (index + 1)
      | '>' -> finish (index + 1)
      | byte ->
          add buffer byte;
          comment_text (index + 1)
  and comment_start_dash index =
    if index >= length then finish index
    else
      match data.[index] with
      | '-' -> comment_end (index + 1)
      | '>' -> finish (index + 1)
      | byte ->
          Buffer.add_char buffer '-';
          add buffer byte;
          comment_text (index + 1)
  and comment_text index =
    if index >= length then finish index
    else
      match data.[index] with
      | '-' -> comment_end_dash (index + 1)
      | byte ->
          add buffer byte;
          comment_text (index + 1)
  and comment_end_dash index =
    if index >= length then finish index
    else
      match data.[index] with
      | '-' -> comment_end (index + 1)
      | byte ->
          Buffer.add_char buffer '-';
          add buffer byte;
          comment_text (index + 1)
  and comment_end index =
    if index >= length then finish index
    else
      match data.[index] with
      | '>' -> finish (index + 1)
      | '!' -> comment_end_bang (index + 1)
      | '-' ->
          Buffer.add_char buffer '-';
          comment_end (index + 1)
      | byte ->
          Buffer.add_string buffer "--";
          add buffer byte;
          comment_text (index + 1)
  and comment_end_bang index =
    if index >= length then finish index
    else
      match data.[index] with
      | '-' ->
          Buffer.add_string buffer "--!";
          comment_end_dash (index + 1)
      | '>' -> finish (index + 1)
      | byte ->
          Buffer.add_string buffer "--!";
          add buffer byte;
          comment_text (index + 1)
  in
  comment_start start

let doctype data start =
  let length = String.length data in
  let name = ref None in
  let public_identifier = ref None in
  let system_identifier = ref None in
  let quirks = ref false in
  let add_to field byte =
    let buffer =
      match !field with
      | Some buffer -> buffer
      | None ->
          let buffer = Buffer.create 32 in
          field := Some buffer;
          buffer
    in
    add buffer byte
  in
  let finish ?(force_quirks = false) index =
    if force_quirks then quirks := true;
    let contents field =
      match !field with
      | None -> None
      | Some buffer -> Some (Buffer.contents buffer)
    in
    {
      token =
        Html_tokenizer.Doctype
          {
            doctype_name = contents name;
            public_identifier = contents public_identifier;
            system_identifier = contents system_identifier;
            raw_text = None;
            force_quirks = !quirks;
          };
      next = index;
    }
  in
  let rec doctype_start index =
    if index >= length then finish ~force_quirks:true index
    else if is_whitespace data.[index] then before_name (index + 1)
    else before_name index
  and before_name index =
    if index >= length then finish ~force_quirks:true index
    else
      let byte = data.[index] in
      if is_whitespace byte then before_name (index + 1)
      else if byte = '>' then finish ~force_quirks:true (index + 1)
      else begin
        add_to name (Char.lowercase_ascii byte);
        name_state (index + 1)
      end
  and name_state index =
    if index >= length then finish ~force_quirks:true index
    else
      let byte = data.[index] in
      if is_whitespace byte then after_name (index + 1)
      else if byte = '>' then finish (index + 1)
      else begin
        add_to name (Char.lowercase_ascii byte);
        name_state (index + 1)
      end
  and after_name index =
    if index >= length then finish ~force_quirks:true index
    else
      let byte = data.[index] in
      if is_whitespace byte then after_name (index + 1)
      else if byte = '>' then finish (index + 1)
      else if matches_lowercase data index "public" then
        after_public_keyword (index + 6)
      else if matches_lowercase data index "system" then
        after_system_keyword (index + 6)
      else begin
        quirks := true;
        bogus index
      end
  and after_public_keyword index =
    if index >= length then finish ~force_quirks:true index
    else
      let byte = data.[index] in
      if is_whitespace byte then before_public_identifier (index + 1)
      else if byte = '"' || byte = '\'' then begin
        public_identifier := Some (Buffer.create 32);
        identifier_quoted public_identifier byte after_public_identifier
          (index + 1)
      end
      else if byte = '>' then finish ~force_quirks:true (index + 1)
      else begin
        quirks := true;
        bogus (index + 1)
      end
  and before_public_identifier index =
    if index >= length then finish ~force_quirks:true index
    else
      let byte = data.[index] in
      if is_whitespace byte then before_public_identifier (index + 1)
      else if byte = '"' || byte = '\'' then begin
        public_identifier := Some (Buffer.create 32);
        identifier_quoted public_identifier byte after_public_identifier
          (index + 1)
      end
      else if byte = '>' then finish ~force_quirks:true (index + 1)
      else begin
        quirks := true;
        bogus (index + 1)
      end
  and identifier_quoted field quote next_state index =
    if index >= length then finish ~force_quirks:true index
    else
      let byte = data.[index] in
      if byte = quote then next_state (index + 1)
      else if byte = '>' then finish ~force_quirks:true (index + 1)
      else begin
        add_to field byte;
        identifier_quoted field quote next_state (index + 1)
      end
  and after_public_identifier index =
    if index >= length then finish ~force_quirks:true index
    else
      let byte = data.[index] in
      if is_whitespace byte then between_identifiers (index + 1)
      else if byte = '>' then finish (index + 1)
      else if byte = '"' || byte = '\'' then begin
        system_identifier := Some (Buffer.create 32);
        identifier_quoted system_identifier byte after_system_identifier
          (index + 1)
      end
      else begin
        quirks := true;
        bogus (index + 1)
      end
  and between_identifiers index =
    if index >= length then finish ~force_quirks:true index
    else
      let byte = data.[index] in
      if is_whitespace byte then between_identifiers (index + 1)
      else if byte = '>' then finish (index + 1)
      else if byte = '"' || byte = '\'' then begin
        system_identifier := Some (Buffer.create 32);
        identifier_quoted system_identifier byte after_system_identifier
          (index + 1)
      end
      else begin
        quirks := true;
        bogus (index + 1)
      end
  and after_system_keyword index =
    if index >= length then finish ~force_quirks:true index
    else
      let byte = data.[index] in
      if is_whitespace byte then before_system_identifier (index + 1)
      else if byte = '"' || byte = '\'' then begin
        system_identifier := Some (Buffer.create 32);
        identifier_quoted system_identifier byte after_system_identifier
          (index + 1)
      end
      else if byte = '>' then finish ~force_quirks:true (index + 1)
      else begin
        quirks := true;
        bogus (index + 1)
      end
  and before_system_identifier index =
    if index >= length then finish ~force_quirks:true index
    else
      let byte = data.[index] in
      if is_whitespace byte then before_system_identifier (index + 1)
      else if byte = '"' || byte = '\'' then begin
        system_identifier := Some (Buffer.create 32);
        identifier_quoted system_identifier byte after_system_identifier
          (index + 1)
      end
      else if byte = '>' then finish ~force_quirks:true (index + 1)
      else begin
        quirks := true;
        bogus (index + 1)
      end
  and after_system_identifier index =
    if index >= length then finish ~force_quirks:true index
    else
      let byte = data.[index] in
      if is_whitespace byte then after_system_identifier (index + 1)
      else if byte = '>' then finish (index + 1)
      else bogus (index + 1)
  and bogus index =
    if index >= length then finish index
    else if data.[index] = '>' then finish (index + 1)
    else bogus (index + 1)
  in
  doctype_start start

let scan data index =
  if data.[index] = '?' then bogus_comment data (index + 1)
  else if
    index + 3 <= String.length data
    && data.[index + 1] = '-'
    && data.[index + 2] = '-'
  then comment data (index + 3)
  else if matches_lowercase data (index + 1) "doctype" then
    doctype data (index + 8)
  else bogus_comment data (index + 1)
