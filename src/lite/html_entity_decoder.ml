(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common
module Trie = Markup_entities.Trie

let named_entity_trie =
  lazy
    (Array.fold_left
       (fun trie (name, characters) -> Trie.add name characters trie)
       (Trie.create ()) Markup_entities.Entities.entities)

let replace_windows_1252_entity = function
  | 0x80 -> 0x20AC
  | 0x82 -> 0x201A
  | 0x83 -> 0x0192
  | 0x84 -> 0x201E
  | 0x85 -> 0x2026
  | 0x86 -> 0x2020
  | 0x87 -> 0x2021
  | 0x88 -> 0x02C6
  | 0x89 -> 0x2030
  | 0x8A -> 0x0160
  | 0x8B -> 0x2039
  | 0x8C -> 0x0152
  | 0x8E -> 0x017D
  | 0x91 -> 0x2018
  | 0x92 -> 0x2019
  | 0x93 -> 0x201C
  | 0x94 -> 0x201D
  | 0x95 -> 0x2022
  | 0x96 -> 0x2013
  | 0x97 -> 0x2014
  | 0x98 -> 0x02DC
  | 0x99 -> 0x2122
  | 0x9A -> 0x0161
  | 0x9B -> 0x203A
  | 0x9C -> 0x0153
  | 0x9E -> 0x017E
  | 0x9F -> 0x0178
  | c -> c

let[@inline] is_decimal c = is_digit (Char.code c)
let[@inline] is_hexadecimal c = is_hex_digit (Char.code c)
let[@inline] is_alphanumeric_char c = is_alphanumeric (Char.code c)

let decode_references in_attribute text =
  let length = String.length text in
  let buffer = Buffer.create length in
  let numeric_value ~hexadecimal digits finish =
    let digits = String.sub text digits (finish - digits) in
    let value =
      int_of_string_opt (if hexadecimal then "0x" ^ digits else digits)
    in
    match value with
    | None -> u_rep
    | Some value ->
        let value = replace_windows_1252_entity value in
        if value = 0 || not (is_scalar value) then u_rep else value
  in
  let rec search copied index =
    if index >= length then
      Buffer.add_substring buffer text copied (length - copied)
    else if text.[index] <> '&' then search copied (index + 1)
    else
      match reference (index + 1) with
      | None -> search copied (index + 1)
      | Some (after, value) ->
          Buffer.add_substring buffer text copied (index - copied);
          begin match value with
          | `One codepoint -> add_utf_8 buffer codepoint
          | `Two (first, second) ->
              add_utf_8 buffer first;
              add_utf_8 buffer second
          end;
          search after after
  and reference start =
    if start >= length then None
    else
      match text.[start] with
      | '\t' | '\n' | '\x0C' | ' ' | '<' | '&' -> None
      | '#' -> numeric_reference (start + 1)
      | _ -> named_reference start
  and numeric_reference start =
    if start < length && (text.[start] = 'x' || text.[start] = 'X') then
      let digits = start + 1 in
      let finish = consume_while digits is_hexadecimal in
      if finish = digits then None
      else
        Some (terminate finish (numeric_value ~hexadecimal:true digits finish))
    else
      let finish = consume_while start is_decimal in
      if finish = start then None
      else
        Some (terminate finish (numeric_value ~hexadecimal:false start finish))
  and terminate finish value =
    if finish < length && text.[finish] = ';' then (finish + 1, `One value)
    else (finish, `One value)
  and named_reference start =
    let rec walk best index trie =
      if index >= length then best
      else
        let trie = Trie.advance (Char.code text.[index]) trie in
        match Trie.matches trie with
        | Trie.No -> best
        | Trie.Prefix -> walk best (index + 1) trie
        | Trie.Multiple value -> walk (Some (index + 1, value)) (index + 1) trie
        | Trie.Yes value -> Some (index + 1, value)
    in
    match walk None start (Lazy.force named_entity_trie) with
    | None -> None
    | Some (name_end, value) ->
        if name_end < length && text.[name_end] = ';' then
          Some (name_end + 1, value)
        else if
          in_attribute && name_end < length
          && (is_alphanumeric_char text.[name_end] || text.[name_end] = '=')
        then None
        else Some (name_end, value)
  and consume_while index predicate =
    if index < length && predicate text.[index] then
      consume_while (index + 1) predicate
    else index
  in
  search 0 0;
  Buffer.contents buffer

let decode text =
  if String.contains text '&' then decode_references false text else text

let decode_attribute text =
  if String.contains text '&' then decode_references true text else text
