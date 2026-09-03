(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

let ascii_lower = Char.lowercase_ascii
let is_space = function ' ' | '\t' | '\n' | '\r' | '\x0C' -> true | _ -> false
let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

type cursor = { input : string; limit : int; mutable position : int }

let next cursor =
  assert (cursor.position < cursor.limit);
  let byte = cursor.input.[cursor.position] in
  cursor.position <- cursor.position + 1;
  byte

let skip_spaces cursor =
  while
    cursor.position < cursor.limit && is_space cursor.input.[cursor.position]
  do
    cursor.position <- cursor.position + 1
  done

(* [text] must contain only lowercase ASCII. *)
let starts_with_ci cursor text =
  let length = String.length text in
  let rec matches index =
    index = length
    || ascii_lower cursor.input.[cursor.position + index] = text.[index]
       && matches (index + 1)
  in
  cursor.position + length <= cursor.limit && matches 0

let read_while cursor predicate =
  let start = cursor.position in
  while
    cursor.position < cursor.limit && predicate cursor.input.[cursor.position]
  do
    cursor.position <- cursor.position + 1
  done;
  String.sub cursor.input start (cursor.position - start)

let read_quoted_value cursor quote =
  let start = cursor.position in
  while
    cursor.position < cursor.limit && cursor.input.[cursor.position] <> quote
  do
    cursor.position <- cursor.position + 1
  done;
  if cursor.position >= cursor.limit then ""
  else
    let value = String.sub cursor.input start (cursor.position - start) in
    cursor.position <- cursor.position + 1;
    String.lowercase_ascii value

let read_unquoted_value cursor terminator =
  read_while cursor (fun byte -> not (is_space byte || byte = terminator))
  |> String.lowercase_ascii

let read_value cursor =
  skip_spaces cursor;
  match next cursor with
  | ('\'' | '"') as quote -> read_quoted_value cursor quote
  | _ ->
      cursor.position <- cursor.position - 1;
      read_unquoted_value cursor '>'

let read_attribute_name cursor =
  let start = cursor.position in
  while
    cursor.position < cursor.limit
    &&
    let byte = cursor.input.[cursor.position] in
    not
      (is_space byte || byte = '/' || byte = '>'
      || (byte = '=' && cursor.position > start))
  do
    cursor.position <- cursor.position + 1
  done;
  String.sub cursor.input start (cursor.position - start)
  |> String.lowercase_ascii

let read_attribute cursor =
  skip_spaces cursor;
  while
    cursor.position < cursor.limit && cursor.input.[cursor.position] = '/'
  do
    cursor.position <- cursor.position + 1;
    skip_spaces cursor
  done;
  if cursor.position >= cursor.limit || cursor.input.[cursor.position] = '>'
  then None
  else
    let name = read_attribute_name cursor in
    skip_spaces cursor;
    let value =
      if cursor.position < cursor.limit && cursor.input.[cursor.position] = '='
      then begin
        cursor.position <- cursor.position + 1;
        if cursor.position < cursor.limit then read_value cursor else ""
      end
      else ""
    in
    Some (name, value)

let extract_encoding value : string option =
  let cursor = { input = value; limit = String.length value; position = 0 } in
  let rec search () =
    if cursor.position >= cursor.limit then None
    else if starts_with_ci cursor "charset" then begin
      cursor.position <- cursor.position + 7;
      skip_spaces cursor;
      if cursor.position >= cursor.limit then None
      else if next cursor <> '=' then begin
        cursor.position <- cursor.position - 1;
        search ()
      end
      else begin
        skip_spaces cursor;
        if cursor.position >= cursor.limit then None
        else
          let value =
            match next cursor with
            | ('\'' | '"') as quote -> read_quoted_value cursor quote
            | _ ->
                cursor.position <- cursor.position - 1;
                read_unquoted_value cursor ';'
          in
          if value = "" then search () else Some value
      end
    end
    else begin
      cursor.position <- cursor.position + 1;
      search ()
    end
  in
  search ()

let normalize_declared_encoding value =
  match String.lowercase_ascii (Common.trim_string value) with
  | "unicode-1-1-utf-8" | "utf-8" | "utf8" -> "utf-8"
  | "utf-16" | "utf-16be" | "utf-16le" -> "utf-8"
  | "cp1251" | "windows-1251" | "x-cp1251" -> "windows-1251"
  | "ansi_x3.4-1968" | "ascii" | "cp1252" | "cp819" | "csisolatin1" | "ibm819"
  | "iso-8859-1" | "iso-ir-100" | "iso8859-1" | "iso88591" | "iso_8859-1"
  | "iso_8859-1:1987" | "l1" | "latin1" | "us-ascii" | "windows-1252"
  | "x-cp1252" ->
      "windows-1252"
  | value -> value

(** Parse encoding inside a [meta] tag *)
let read_meta_encoding cursor =
  let rec attributes names got_pragma need_pragma charset =
    match read_attribute cursor with
    | None ->
        if need_pragma = Some true && not got_pragma then None
        else Option.map normalize_declared_encoding charset
    | Some (name, _) when Common.list_mem_string name names ->
        attributes names got_pragma need_pragma charset
    | Some (name, value) ->
        let names = name :: names in
        begin match name with
        | "http-equiv" ->
            attributes names
              (got_pragma || value = "content-type")
              need_pragma charset
        | "content" when charset = None ->
            begin match extract_encoding value with
            | Some value -> attributes names got_pragma (Some true) (Some value)
            | None -> attributes names got_pragma need_pragma charset
            end
        | "charset" when value <> "" ->
            attributes names got_pragma (Some false) (Some value)
        | _ -> attributes names got_pragma need_pragma charset
        end
  in
  attributes [] false None None

let skip_tag_like cursor =
  while cursor.position < cursor.limit && next cursor <> '>' do
    ()
  done

let skip_tag cursor =
  while
    cursor.position < cursor.limit
    && (not (is_space cursor.input.[cursor.position]))
    && cursor.input.[cursor.position] <> '>'
  do
    cursor.position <- cursor.position + 1
  done;
  while cursor.position < cursor.limit && read_attribute cursor <> None do
    ()
  done;
  if cursor.position < cursor.limit then cursor.position <- cursor.position + 1

let skip_comment cursor =
  while
    cursor.position + 2 < cursor.limit
    && (cursor.input.[cursor.position] <> '-'
       || cursor.input.[cursor.position + 1] <> '-'
       || cursor.input.[cursor.position + 2] <> '>')
  do
    cursor.position <- cursor.position + 1
  done;
  cursor.position <- min cursor.limit (cursor.position + 3)

let declared_encoding input : string option =
  let cursor =
    { input; limit = min 1024 (String.length input); position = 0 }
  in
  let rec scan () =
    while cursor.position < cursor.limit && next cursor <> '<' do
      ()
    done;
    if cursor.position >= cursor.limit then None
    else if starts_with_ci cursor "!--" then begin
      cursor.position <- cursor.position + 3;
      skip_comment cursor;
      scan ()
    end
    else if starts_with_ci cursor "meta" then begin
      cursor.position <- cursor.position + 4;
      if
        cursor.position < cursor.limit
        && (is_space cursor.input.[cursor.position]
           || cursor.input.[cursor.position] = '/')
      then (
        match read_meta_encoding cursor with
        | Some _ as encoding -> encoding
        | None ->
            if cursor.position < cursor.limit then
              cursor.position <- cursor.position + 1;
            scan ())
      else begin
        skip_tag cursor;
        scan ()
      end
    end
    else if cursor.position < cursor.limit then begin
      begin match cursor.input.[cursor.position] with
      | '!' | '?' -> skip_tag_like cursor
      | '/'
        when cursor.position + 1 < cursor.limit
             && is_letter cursor.input.[cursor.position + 1] ->
          skip_tag cursor
      | '/' -> skip_tag_like cursor
      | byte when is_letter byte -> skip_tag cursor
      | _ -> ()
      end;
      scan ()
    end
    else None
  in
  scan ()

let transcode scalar input =
  let output = Buffer.create (String.length input + 32) in
  String.iter
    (fun byte ->
      Uutf.Buffer.add_utf_8 output (Uchar.of_int (scalar (Char.code byte))))
    input;
  Buffer.contents output

module Windows_1252 = struct
  let high =
    [|
      0x20AC;
      0x0081;
      0x201A;
      0x0192;
      0x201E;
      0x2026;
      0x2020;
      0x2021;
      0x02C6;
      0x2030;
      0x0160;
      0x2039;
      0x0152;
      0x008D;
      0x017D;
      0x008F;
      0x0090;
      0x2018;
      0x2019;
      0x201C;
      0x201D;
      0x2022;
      0x2013;
      0x2014;
      0x02DC;
      0x2122;
      0x0161;
      0x203A;
      0x0153;
      0x009D;
      0x017E;
      0x0178;
    |]

  let decode input =
    transcode
      (fun byte ->
        if byte < 0x80 || byte >= 0xA0 then byte else high.(byte - 0x80))
      input
end

let windows_1251_high =
  [|
    0x0402;
    0x0403;
    0x201A;
    0x0453;
    0x201E;
    0x2026;
    0x2020;
    0x2021;
    0x20AC;
    0x2030;
    0x0409;
    0x2039;
    0x040A;
    0x040C;
    0x040B;
    0x040F;
    0x0452;
    0x2018;
    0x2019;
    0x201C;
    0x201D;
    0x2022;
    0x2013;
    0x2014;
    0xFFFD;
    0x2122;
    0x0459;
    0x203A;
    0x045A;
    0x045C;
    0x045B;
    0x045F;
    0x00A0;
    0x040E;
    0x045E;
    0x0408;
    0x00A4;
    0x0490;
    0x00A6;
    0x00A7;
    0x0401;
    0x00A9;
    0x0404;
    0x00AB;
    0x00AC;
    0x00AD;
    0x00AE;
    0x0407;
    0x00B0;
    0x00B1;
    0x0406;
    0x0456;
    0x0491;
    0x00B5;
    0x00B6;
    0x00B7;
    0x0451;
    0x2116;
    0x0454;
    0x00BB;
    0x0458;
    0x0405;
    0x0455;
    0x0457;
  |]

let windows_1251 input =
  transcode
    (fun byte ->
      if byte < 0x80 then byte
      else if byte < 0xC0 then windows_1251_high.(byte - 0x80)
        (* Match the baseline's long-standing Windows-1251 table exactly. *)
      else if byte = 0xD0 then 0x0410
      else 0x0410 + byte - 0xC0)
    input

let transcode_utf_16 fold input =
  let output = Buffer.create (String.length input) in
  fold
    (fun () _ -> function
      | `Uchar uchar -> Uutf.Buffer.add_utf_8 output uchar
      | `Malformed _ -> Uutf.Buffer.add_utf_8 output Uutf.u_rep)
    () input;
  Buffer.contents output

let utf_16be input =
  transcode_utf_16
    (fun folder state input -> Uutf.String.fold_utf_16be folder state input)
    input

let utf_16le input =
  transcode_utf_16
    (fun folder state input -> Uutf.String.fold_utf_16le folder state input)
    input

let bom input =
  let length = String.length input in
  if length >= 2 && input.[0] = '\xFE' && input.[1] = '\xFF' then Some `UTF_16BE
  else if length >= 2 && input.[0] = '\xFF' && input.[1] = '\xFE' then
    Some `UTF_16LE
  else if
    length >= 3
    && input.[0] = '\xEF'
    && input.[1] = '\xBB'
    && input.[2] = '\xBF'
  then Some `UTF_8
  else None

let decode_html input : string =
  match bom input with
  | Some `UTF_16BE -> utf_16be input
  | Some `UTF_16LE -> utf_16le input
  | Some `UTF_8 -> input
  | None -> (
      match declared_encoding input with
      | Some "windows-1251" -> windows_1251 input
      | Some "windows-1252" -> Windows_1252.decode input
      | _ -> input)
