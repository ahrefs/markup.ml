(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

type 'a cont = 'a -> unit
type 'a cps = exn cont -> 'a cont -> unit

type location = Markup_common.location

let compare_locations = Markup_common.compare_locations

type name = Markup_common.name

let xml_ns = Markup_common.Ns.xml
let xmlns_ns = Markup_common.Ns.xmlns
let xlink_ns = Markup_common.Ns.xlink
let html_ns = Markup_common.Ns.html
let svg_ns = Markup_common.Ns.svg
let mathml_ns = Markup_common.Ns.mathml

module Token_tag =
struct
  type t =
    {name         : string;
     attributes   : (string * string) list;
     self_closing : bool}
end

type xml_declaration = Markup_common.xml_declaration = {
  version : string;
  encoding : string option;
  standalone : bool option;
}

type doctype = Markup_common.doctype = {
  doctype_name : string option;
  public_identifier : string option;
  system_identifier : string option;
  raw_text : string option;
  force_quirks : bool;
}

type signal = Markup_common.signal

type general_token =
  [ `Xml of xml_declaration
  | `Doctype of doctype
  | `Start of Token_tag.t
  | `End of Token_tag.t
  | `Chars of string list
  | `Char of int
  | `PI of string * string
  | `Comment of string
  | `EOF ]

let u_rep = Uchar.to_int Uutf.u_rep

let add_utf_8 buffer c =
  Uutf.Buffer.add_utf_8 buffer (Uchar.unsafe_of_int c)

let format_char = Printf.sprintf "U+%04X"

(* Type constraints are necessary to avoid polymorphic comparison, which would
   greatly reduce performance: https://github.com/aantron/markup.ml/pull/15. *)
let is_in_range (lower : int) (upper : int) c = c >= lower && c <= upper

(* HTML 8.2.2.5. *)
let is_control_character = function
  | 0x000B -> true
  | c when is_in_range 0x0001 0x0008 c -> true
  | c when is_in_range 0x000E 0x001F c -> true
  | c when is_in_range 0x007F 0x009F c -> true
  | _ -> false

(* HTML 8.2.2.5. *)
let is_non_character = function
  | c when is_in_range 0xFDD0 0xFDEF c -> true
  | c when (c land 0xFFFF = 0xFFFF) || (c land 0xFFFF = 0xFFFE) -> true
  | _ -> false

let is_digit = is_in_range 0x0030 0x0039

let is_hex_digit = function
  | c when is_digit c -> true
  | c when is_in_range 0x0041 0x0046 c -> true
  | c when is_in_range 0x0061 0x0066 c -> true
  | _ -> false

let is_scalar = function
  | c when (c >= 0x10FFFF) || ((c >= 0xD800) && (c <= 0xDFFF)) -> false
  | _ -> true

let is_uppercase = is_in_range 0x0041 0x005A

let is_lowercase = is_in_range 0x0061 0x007A

let is_alphabetic = function
  | c when is_uppercase c -> true
  | c when is_lowercase c -> true
  | _ -> false

let is_alphanumeric = function
  | c when is_alphabetic c -> true
  | c when is_digit c -> true
  | _ -> false

let is_whitespace c = c = 0x0020 || c = 0x000A || c = 0x0009 || c = 0x000D

let is_whitespace_only s =
  try
    s |> String.iter (fun c ->
      if is_whitespace (int_of_char c) then ()
      else raise Exit);
    true

  with Exit -> false

let to_lowercase = function
  | c when is_uppercase c -> c + 0x20
  | c -> c

let is_printable = is_in_range 0x0020 0x007E

let char c =
  if is_printable c then begin
    let buffer = Buffer.create 4 in
    add_utf_8 buffer c;
    Buffer.contents buffer
  end
  else
    format_char c

let is_valid_html_char c = not (is_control_character c || is_non_character c)

let is_valid_xml_char c =
  is_in_range 0x0020 0xD7FF c
  || c = 0x0009
  || c = 0x000A
  || c = 0x000D
  || is_in_range 0xE000 0xFFFD c
  || is_in_range 0x10000 0x10FFFF c

let signal_to_string = Markup_common.signal_to_string

let token_to_string = function
  | `Xml x ->
    signal_to_string (`Xml x)

  | `Doctype d ->
    signal_to_string (`Doctype d)

  | `Start t ->
    let name = "", t.Token_tag.name in
    let attributes =
      t.Token_tag.attributes |> List.map (fun (n, v) -> ("", n), v) in
    let s = signal_to_string (`Start_element (name, attributes)) in
    if not t.Token_tag.self_closing then s
    else (String.sub s 0 (String.length s - 1)) ^ "/>"

  | `End t ->
    Printf.sprintf "</%s>" t.Token_tag.name

  | `Chars ss ->
    String.concat "" ss

  | `Char i ->
    char i

  | `String s -> s

  | `PI v ->
    signal_to_string (`PI v)

  | `Comment s ->
    signal_to_string (`Comment s)

  | `EOF ->
    "EOF"

let whitespace_chars = " \t\n\r"

let whitespace_prefix_length s =
  let rec loop index =
    if index = String.length s then index
    else
      if String.contains whitespace_chars s.[index] then loop (index + 1)
      else index
  in
  loop 0

let whitespace_suffix_length s =
  let rec loop rindex =
    if rindex = String.length s then rindex
    else
      if String.contains whitespace_chars s.[String.length s - rindex - 1] then
        loop (rindex + 1)
      else rindex
  in
  loop 0

let trim_string_left s =
  let prefix_length = whitespace_prefix_length s in
  String.sub s prefix_length (String.length s - prefix_length)

let trim_string_right s =
  let suffix_length = whitespace_suffix_length s in
  String.sub s 0 (String.length s - suffix_length)

(* String.trim not available for OCaml < 4.00. *)
let trim_string s = s |> trim_string_left |> trim_string_right

(* Specialization of List.mem at string list, to avoid polymorphic
   comparison. *)
let list_mem_string (s : string) l = List.exists (fun s' -> s' = s) l
