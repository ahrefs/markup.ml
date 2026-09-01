(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

let ascii_lower = Char.lowercase_ascii
let is_space = function ' ' | '\t' | '\n' | '\r' | '\x0C' -> true | _ -> false

let declared_encoding input =
  let length = min 1024 (String.length input) in
  let needle = "charset" in
  let rec matches index offset =
    offset = String.length needle
    || index + offset < length
       && ascii_lower input.[index + offset] = needle.[offset]
       && matches index (offset + 1)
  in
  let rec skip_spaces index =
    if index < length && is_space input.[index] then skip_spaces (index + 1)
    else index
  in
  let rec value_end index =
    if index < length then
      match input.[index] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> value_end (index + 1)
      | _ -> index
    else index
  in
  let rec search index =
    if index + String.length needle > length then None
    else if not (matches index 0) then search (index + 1)
    else
      let after_name = skip_spaces (index + String.length needle) in
      if after_name >= length || input.[after_name] <> '=' then
        search (index + 1)
      else
        let start = skip_spaces (after_name + 1) in
        let start =
          if start < length && (input.[start] = '\'' || input.[start] = '"')
          then start + 1
          else start
        in
        let stop = value_end start in
        if stop = start then search (index + 1)
        else
          Some (String.sub input start (stop - start) |> String.lowercase_ascii)
  in
  search 0

let windows_1252_high =
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

let transcode scalar input =
  let output = Buffer.create (String.length input + 32) in
  String.iter
    (fun byte ->
      Uutf.Buffer.add_utf_8 output (Uchar.of_int (scalar (Char.code byte))))
    input;
  Buffer.contents output

let windows_1252 input =
  transcode
    (fun byte ->
      if byte < 0x80 || byte >= 0xA0 then byte
      else windows_1252_high.(byte - 0x80))
    input

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

let decode_html input =
  match bom input with
  | Some `UTF_16BE -> utf_16be input
  | Some `UTF_16LE -> utf_16le input
  | Some `UTF_8 -> input
  | None -> (
      match declared_encoding input with
      | Some ("windows-1251" | "cp1251" | "x-cp1251") -> windows_1251 input
      | Some
          ( "windows-1252" | "cp1252" | "x-cp1252" | "iso-8859-1" | "latin1"
          | "us-ascii" | "ascii" ) ->
          windows_1252 input
      | _ -> input)
