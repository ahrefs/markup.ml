(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

let rec ascii_attribute_safe s index =
  if index = String.length s then true
  else
    match s.[index] with
    | '&' | '"' | '\x80' .. '\xFF' -> false
    | _ -> ascii_attribute_safe s (index + 1)

let escape_attribute_slow ~into:buffer s =
  Uutf.String.fold_utf_8
    (fun () _ -> function
      | `Malformed _ -> ()
      | `Uchar c -> (
          match Uchar.to_int c with
          | 0x0026 -> Buffer.add_string buffer "&amp;"
          | 0x00A0 -> Buffer.add_string buffer "&nbsp;"
          | 0x0022 -> Buffer.add_string buffer "&quot;"
          | c -> add_utf_8 buffer c))
    () s

let escape_attribute buffer s =
  if ascii_attribute_safe s 0 then Buffer.add_string buffer s
  else escape_attribute_slow ~into:buffer s

let rec ascii_text_safe s index =
  if index = String.length s then true
  else
    match s.[index] with
    | '&' | '<' | '>' | '\x80' .. '\xFF' -> false
    | _ -> ascii_text_safe s (index + 1)

let escape_text_slow ~into:buffer s =
  Uutf.String.fold_utf_8
    (fun () _ -> function
      | `Malformed _ -> ()
      | `Uchar c -> (
          match Uchar.to_int c with
          | 0x0026 -> Buffer.add_string buffer "&amp;"
          | 0x00A0 -> Buffer.add_string buffer "&nbsp;"
          | 0x003C -> Buffer.add_string buffer "&lt;"
          | 0x003E -> Buffer.add_string buffer "&gt;"
          | c -> add_utf_8 buffer c))
    () s

let escape_text buffer s =
  if ascii_text_safe s 0 then Buffer.add_string buffer s
  else escape_text_slow ~into:buffer s

let void_elements =
  [
    "area";
    "base";
    "basefont";
    "bgsound";
    "br";
    "col";
    "embed";
    "frame";
    "hr";
    "img";
    "input";
    "keygen";
    "link";
    "meta";
    "param";
    "source";
    "track";
    "wbr";
  ]

let prepend_newline_for = [ "pre"; "textarea"; "listing" ]

let rec starts_with_newline = function
  | [] -> false
  | s :: more ->
      if String.length s = 0 then starts_with_newline more else s.[0] = '\x0A'

let literal_text_elements =
  [ "style"; "script"; "xmp"; "iframe"; "noembed"; "noframes"; "plaintext" ]

let element_name = function
  | ns, local_name when list_mem_string ns [ html_ns; svg_ns; mathml_ns ] ->
      local_name
  | ns, local_name when ns = xml_ns -> "xml:" ^ local_name
  | ns, local_name when ns = xmlns_ns -> "xmlns:" ^ local_name
  | ns, local_name when ns = xlink_ns -> "xlink:" ^ local_name
  | _, local_name -> local_name

let attribute_name = function
  | "", local_name -> local_name
  | ns, local_name when ns = xml_ns -> "xml:" ^ local_name
  | ns, "xmlns" when ns = xmlns_ns -> "xmlns"
  | ns, local_name when ns = xmlns_ns -> "xmlns:" ^ local_name
  | ns, local_name when ns = xlink_ns -> "xlink:" ^ local_name
  | _, local_name -> local_name

let write ?(escape_attribute = escape_attribute) ?(escape_text = escape_text)
    buffer stream =
  let signals = Markup_common.Stream.Private.of_stream stream in
  let open_elements = ref [] in
  let pending = ref None in

  let in_literal_text_element () =
    match !open_elements with
    | element :: _ -> list_mem_string element literal_text_elements
    | [] -> false
  in

  let rec next throw ended k =
    match !pending with
    | Some signal ->
        pending := None;
        k signal
    | None -> Kstream.next signals throw ended k
  and peek throw ended k =
    next throw ended (fun signal ->
        pending := Some signal;
        k signal)
  and loop throw ended =
    next throw ended (fun signal ->
        match signal with
        | `Start_element (((ns, local_name) as name), attributes) ->
            let tag_name = element_name name in
            Buffer.add_char buffer '<';
            Buffer.add_string buffer tag_name;
            List.iter
              (fun (name, value) ->
                Buffer.add_char buffer ' ';
                Buffer.add_string buffer (attribute_name name);
                Buffer.add_string buffer "=\"";
                escape_attribute buffer value;
                Buffer.add_char buffer '"')
              attributes;
            Buffer.add_char buffer '>';

            if ns = html_ns && list_mem_string local_name void_elements then
              peek throw
                (fun () -> ended ())
                (function
                  | `End_element ->
                      next throw
                        (fun () -> assert false)
                        (fun _ -> loop throw ended)
                  | `Start_element _ | `Text _ | `Comment _ | `PI _ | `Xml _
                  | `Doctype _ ->
                      open_elements := tag_name :: !open_elements;
                      loop throw ended)
            else begin
              open_elements := tag_name :: !open_elements;
              if ns = html_ns && list_mem_string local_name prepend_newline_for
              then
                peek throw
                  (fun () -> ended ())
                  (function
                    | `Text strings when starts_with_newline strings ->
                        Buffer.add_char buffer '\n';
                        loop throw ended
                    | `Text _ | `Start_element _ | `End_element | `Comment _
                    | `PI _ | `Doctype _ | `Xml _ ->
                        loop throw ended)
              else loop throw ended
            end
        | `End_element ->
            begin match !open_elements with
            | [] -> loop throw ended
            | name :: rest ->
                open_elements := rest;
                Buffer.add_string buffer "</";
                Buffer.add_string buffer name;
                Buffer.add_char buffer '>';
                loop throw ended
            end
        | `Text strings ->
            if List.for_all (fun s -> String.length s = 0) strings then
              loop throw ended
            else begin
              if in_literal_text_element () then
                List.iter (Buffer.add_string buffer) strings
              else List.iter (escape_text buffer) strings;
              loop throw ended
            end
        | `Comment s ->
            Buffer.add_string buffer "<!--";
            Buffer.add_string buffer s;
            Buffer.add_string buffer "-->";
            loop throw ended
        | `PI (target, s) ->
            Buffer.add_string buffer "<?";
            Buffer.add_string buffer target;
            Buffer.add_char buffer ' ';
            Buffer.add_string buffer s;
            Buffer.add_char buffer '>';
            loop throw ended
        | `Doctype _ as doctype ->
            Buffer.add_string buffer (signal_to_string doctype);
            loop throw ended
        | `Xml _ -> loop throw ended)
  in

  loop raise (fun () -> ())
