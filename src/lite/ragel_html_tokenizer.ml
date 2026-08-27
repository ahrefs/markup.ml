(* Derived from Devkit htmlStream_ragel.ml.rl.
   Devkit is distributed under LGPL-2.1-only with the OCaml linking exception.
   The original source is available from https://github.com/ygrek/ocaml-webstack. *)

[@@@ocaml.warning "-38-32"]

open Common
open Html_tokenizer

type location_out = { mutable line : int; mutable column : int }

type t = {
  data : string;
  cs : int ref;
  p : int ref;
  pe : int ref;
  eof : int ref;
  mark : int ref;
  mark_end : int ref;
  tag : string ref;
  key : string ref;
  attrs : (string * string) list ref;
  mutable declaration : int;
  mutable raw_text : int;
  mutable line : int;
  tokens : Html_tokenizer.token array;
  lines : int array;
  mutable read : int;
  mutable write : int;
  mutable finished : bool;
}

let decode = Html_entity_decoder.decode

(* [attrs] is accumulated in reverse source order; the first occurrence of a
   name wins, like src/baseline. *)
let attributes attrs =
  let rec dedupe seen = function
    | [] -> []
    | (name, value) :: rest ->
        if List.mem name seen then dedupe seen rest
        else
          (name, Html_entity_decoder.decode_attribute value)
          :: dedupe (name :: seen) rest
  in
  dedupe [] (List.rev attrs)

let make_tag ?(self_closing = false) name attributes =
  { Token_tag.name; attributes; self_closing }

let buffer_capacity = 128
let maximum_transition_output = 3

let emit scanner token =
  scanner.tokens.(scanner.write) <- token;
  scanner.lines.(scanner.write) <- scanner.line;
  scanner.write <- scanner.write + 1

let emit_many scanner tokens = List.iter (emit scanner) tokens

(* The tree builder treats a leading whitespace run differently from the rest
   of a text run in several insertion modes; src/baseline gets this for free
   from per-character tokens. *)
let emit_text scanner text =
  let length = String.length text in
  let rec whitespace_end index =
    if index < length then
      match text.[index] with
      | '\t' | '\n' | '\x0C' | '\r' | ' ' -> whitespace_end (index + 1)
      | _ -> index
    else index
  in
  let boundary = whitespace_end 0 in
  if boundary = 0 || boundary = length then emit scanner (String text)
  else begin
    emit scanner (String (String.sub text 0 boundary));
    emit scanner (String (String.sub text boundary (length - boundary)))
  end

let _htmlstream_trans_keys : int array =
  Array.concat
    [
      [|
        10;
        60;
        10;
        60;
        0;
        122;
        10;
        10;
        0;
        62;
        0;
        122;
        0;
        122;
        0;
        122;
        0;
        62;
        10;
        60;
        0;
        62;
        0;
        62;
        10;
        60;
        10;
        34;
        10;
        34;
        0;
        122;
        10;
        39;
        10;
        39;
        0;
        122;
        10;
        62;
        0;
        62;
        10;
        62;
        10;
        10;
        0;
      |];
    ]

let _htmlstream_key_spans : int array =
  Array.concat
    [
      [|
        51;
        51;
        123;
        1;
        63;
        123;
        123;
        123;
        63;
        51;
        63;
        63;
        51;
        25;
        25;
        123;
        30;
        30;
        123;
        53;
        63;
        53;
        1;
      |];
    ]

let _htmlstream_index_offsets : int array =
  Array.concat
    [
      [|
        0;
        52;
        104;
        228;
        230;
        294;
        418;
        542;
        666;
        730;
        782;
        846;
        910;
        962;
        988;
        1014;
        1138;
        1169;
        1200;
        1324;
        1378;
        1442;
        1496;
      |];
    ]

let _htmlstream_indicies : int array =
  Array.concat
    [
      [|
        1;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        0;
        2;
        0;
        4;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        5;
        3;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        7;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        2;
        8;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        9;
        9;
        10;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        6;
        6;
        6;
        6;
        8;
        6;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        6;
        6;
        6;
        6;
        9;
        6;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        9;
        6;
        12;
        11;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        15;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        14;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        16;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        13;
        17;
        13;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        19;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        18;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        20;
        20;
        21;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        6;
        6;
        6;
        22;
        6;
        6;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        6;
        6;
        6;
        6;
        20;
        6;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        20;
        6;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        24;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        23;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        25;
        25;
        26;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        6;
        6;
        27;
        28;
        6;
        6;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        6;
        6;
        6;
        6;
        25;
        6;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        25;
        6;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        30;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        29;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        31;
        31;
        32;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        6;
        6;
        33;
        34;
        6;
        6;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        6;
        6;
        6;
        6;
        31;
        6;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        6;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        35;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        21;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        36;
        6;
        38;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        37;
        39;
        37;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        41;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        33;
        40;
        42;
        40;
        40;
        40;
        40;
        43;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        40;
        6;
        40;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        46;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        45;
        44;
        6;
        44;
        44;
        44;
        44;
        6;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        44;
        47;
        44;
        49;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        48;
        50;
        48;
        52;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        51;
        53;
        51;
        55;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        54;
        56;
        54;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        58;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        57;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        6;
        31;
        31;
        32;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        6;
        6;
        6;
        34;
        6;
        6;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        6;
        6;
        6;
        6;
        31;
        6;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        31;
        6;
        60;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        59;
        53;
        59;
        62;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        61;
        56;
        61;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        65;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        64;
        63;
        63;
        63;
        63;
        63;
        63;
        63;
        63;
        63;
        63;
        63;
        63;
        66;
        66;
        63;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        63;
        63;
        63;
        67;
        63;
        63;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        63;
        63;
        63;
        63;
        66;
        63;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        66;
        63;
        69;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        68;
        70;
        68;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        73;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        72;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        72;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        71;
        74;
        71;
        76;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        75;
        77;
        75;
        79;
        78;
        0;
      |];
    ]

let _htmlstream_trans_targs : int array =
  Array.concat
    [
      [|
        1;
        1;
        2;
        1;
        1;
        2;
        3;
        2;
        0;
        4;
        18;
        3;
        3;
        4;
        5;
        5;
        8;
        12;
        5;
        5;
        6;
        8;
        12;
        7;
        7;
        6;
        8;
        10;
        12;
        7;
        7;
        6;
        8;
        10;
        12;
        8;
        9;
        1;
        1;
        2;
        11;
        10;
        13;
        16;
        11;
        5;
        5;
        12;
        1;
        1;
        2;
        14;
        14;
        15;
        14;
        14;
        15;
        5;
        5;
        17;
        17;
        17;
        17;
        19;
        18;
        18;
        20;
        0;
        19;
        19;
        0;
        20;
        19;
        19;
        0;
        21;
        21;
        22;
        22;
        22;
      |];
    ]

let _htmlstream_trans_actions : int array =
  Array.concat
    [
      [|
        1;
        2;
        0;
        0;
        4;
        3;
        5;
        4;
        6;
        7;
        8;
        0;
        4;
        0;
        9;
        10;
        9;
        9;
        0;
        4;
        1;
        0;
        0;
        11;
        12;
        0;
        13;
        11;
        13;
        0;
        4;
        14;
        15;
        0;
        15;
        4;
        0;
        17;
        18;
        16;
        1;
        4;
        0;
        0;
        0;
        19;
        20;
        19;
        22;
        23;
        21;
        1;
        2;
        24;
        0;
        4;
        25;
        15;
        26;
        1;
        2;
        0;
        4;
        27;
        0;
        4;
        1;
        27;
        0;
        4;
        0;
        0;
        28;
        29;
        28;
        0;
        4;
        30;
        0;
        4;
      |];
    ]

let _htmlstream_eof_actions : int array =
  Array.concat
    [
      [|
        0; 3; 5; 0; 5; 5; 5; 5; 5; 16; 5; 5; 21; 5; 5; 5; 5; 5; 5; 5; 5; 0; 0;
      |];
    ]

let htmlstream_start : int = 0
let htmlstream_first_final : int = 0
let htmlstream_error : int = -1
let htmlstream_en_garbage_tag : int = 21
let htmlstream_en_main : int = 0

type _htmlstream_state = { mutable keys : int; mutable trans : int }

exception Goto_match_htmlstream
exception Goto_again_htmlstream
exception Goto_eof_trans_htmlstream

let create data =
  let cs = ref 0 in

  begin
    cs.contents <- htmlstream_start
  end;

  let length = String.length data in
  {
    data;
    cs;
    p = ref 0;
    pe = ref length;
    eof = ref length;
    mark = ref (-1);
    mark_end = ref (-1);
    tag = ref "";
    key = ref "";
    attrs = ref [];
    declaration = -1;
    raw_text = -1;
    line = 1;
    tokens = Array.make buffer_capacity EOF;
    lines = Array.make buffer_capacity 1;
    read = 0;
    write = 0;
    finished = false;
  }

let run scanner =
  let data = scanner.data in
  let cs = scanner.cs in
  let p = scanner.p in
  let pe = scanner.pe in
  let eof = scanner.eof in
  let mark = scanner.mark in
  let mark_end = scanner.mark_end in
  let tag = scanner.tag in
  let key = scanner.key in
  let attrs = scanner.attrs in
  pe := !eof;
  let pause () =
    if scanner.write >= buffer_capacity - maximum_transition_output && !p < !eof
    then pe := !p + 1
  in
  let substr = String.sub in
  let sub () =
    assert (!mark >= 0);
    if !mark_end < 0 then mark_end := !p;
    let text =
      if !mark_end <= !mark then "" else substr data !mark (!mark_end - !mark)
    in
    mark := -1;
    mark_end := -1;
    text
  in
  if scanner.raw_text >= 0 then begin
    let start = scanner.raw_text in
    scanner.raw_text <- -1;
    let name = !tag in
    let result = Raw_text.scan data start name in
    emit scanner (Start (make_tag name (attributes !attrs)));
    emit scanner (String result.Raw_text.text);
    if result.Raw_text.had_end_tag then emit scanner (End (make_tag name []));
    for index = start to result.Raw_text.next - 1 do
      if data.[index] = '\n' then scanner.line <- scanner.line + 1
    done;
    p := result.Raw_text.next;
    cs := htmlstream_en_main;
    if !p >= !eof then scanner.finished <- true
  end
  else if scanner.declaration >= 0 then begin
    let start = scanner.declaration in
    scanner.declaration <- -1;
    let result = Markup_declaration.scan data start in
    emit scanner result.Markup_declaration.token;
    for index = start to result.Markup_declaration.next - 1 do
      if data.[index] = '\n' then scanner.line <- scanner.line + 1
    done;
    p := result.Markup_declaration.next;
    cs := htmlstream_en_main;
    if !p >= !eof then scanner.finished <- true
  end
  else begin
    begin
      let state = { keys = 0; trans = 0 } in
      let rec do_start () =
        if p.contents = pe.contents then do_test_eof () else do_resume ()
      and do_resume () =
        begin try
          let keys = cs.contents lsl 1 in
          let inds = _htmlstream_index_offsets.(cs.contents) in

          let slen = _htmlstream_key_spans.(cs.contents) in
          state.trans <-
            _htmlstream_indicies.(inds
                                  +
                                  if
                                    slen > 0
                                    && _htmlstream_trans_keys.(keys)
                                       <= Char.code data.[p.contents]
                                    && Char.code data.[p.contents]
                                       <= _htmlstream_trans_keys.(keys + 1)
                                  then
                                    Char.code data.[p.contents]
                                    - _htmlstream_trans_keys.(keys)
                                  else slen)
        with Goto_match_htmlstream -> ()
        end;
        do_eof_trans ()
      and do_eof_trans () =
        cs.contents <- _htmlstream_trans_targs.(state.trans);

        begin try
          if _htmlstream_trans_actions.(state.trans) = 0 then
            raise_notrace Goto_again_htmlstream;

          match _htmlstream_trans_actions.(state.trans) with
          | 1 ->
              begin
                mark := !p
              end;
              ()
          | 25 ->
              begin
                mark_end := !p
              end;
              ()
          | 9 ->
              begin
                tag := String.lowercase_ascii @@ sub ();
                attrs := []
              end;
              ()
          | 28 ->
              begin
                let name = String.lowercase_ascii @@ sub () in
                emit scanner (End (make_tag name []));
                pause ()
              end;
              ()
          | 3 ->
              begin
                emit_text scanner (decode (sub ()));
                pause ()
              end;
              ()
          | 11 ->
              begin
                key := String.lowercase_ascii @@ sub ()
              end;
              ()
          | 15 ->
              begin
                attrs := (!key, if !mark < 0 then "" else sub ()) :: !attrs
              end;
              ()
          | 21 ->
              begin match !tag with
              | "" -> ()
              | "script" | "style" | "title" | "textarea" ->
                  scanner.raw_text <- !p;
                  p.contents <- p.contents - 1;
                  pe := !p + 1
              | name ->
                  emit scanner (Start (make_tag name (attributes !attrs)));
                  pause ()
              end;
              ()
          | 16 ->
              begin match !tag with
              | "" -> ()
              | "script" | "style" | "title" | "textarea" ->
                  scanner.raw_text <- !p;
                  p.contents <- p.contents - 1;
                  pe := !p + 1
              | name ->
                  emit scanner
                    (Start
                       (make_tag ~self_closing:true name (attributes !attrs)));
                  pause ()
              end;
              ()
          | 30 ->
              begin match !tag with
              | "" -> begin
                  cs.contents <- 0;
                  if true then raise_notrace Goto_again_htmlstream
                end
              | "script" | "style" | "title" | "textarea" ->
                  scanner.raw_text <- !p + 1;
                  pe := !p + 1
              | name ->
                  emit scanner (Start (make_tag name (attributes !attrs)));
                  pause ();
                  begin
                    cs.contents <- 0;
                    if true then raise_notrace Goto_again_htmlstream
                  end
              end;
              ()
          | 5 ->
              begin
                p.contents <- p.contents - 1;
                begin
                  cs.contents <- 21;
                  if true then raise_notrace Goto_again_htmlstream
                end
              end;
              ()
          | 4 ->
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | 8 ->
              begin
                tag := ""
              end;
              ()
          | 24 ->
              begin
                mark := !p
              end;
              begin
                mark_end := !p
              end;
              ()
          | 27 ->
              begin
                mark := !p
              end;
              begin
                let name = String.lowercase_ascii @@ sub () in
                emit scanner (End (make_tag name []));
                pause ()
              end;
              ()
          | 2 ->
              begin
                mark := !p
              end;
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | 19 ->
              begin
                mark_end := !p
              end;
              begin
                attrs := (!key, if !mark < 0 then "" else sub ()) :: !attrs
              end;
              ()
          | 10 ->
              begin
                tag := String.lowercase_ascii @@ sub ();
                attrs := []
              end;
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | 29 ->
              begin
                let name = String.lowercase_ascii @@ sub () in
                emit scanner (End (make_tag name []));
                pause ()
              end;
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | 13 ->
              begin
                key := String.lowercase_ascii @@ sub ()
              end;
              begin
                attrs := (!key, if !mark < 0 then "" else sub ()) :: !attrs
              end;
              ()
          | 12 ->
              begin
                key := String.lowercase_ascii @@ sub ()
              end;
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | 14 ->
              begin
                attrs := (!key, if !mark < 0 then "" else sub ()) :: !attrs
              end;
              begin
                mark := !p
              end;
              ()
          | 26 ->
              begin
                attrs := (!key, if !mark < 0 then "" else sub ()) :: !attrs
              end;
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | 22 ->
              begin match !tag with
              | "" -> ()
              | "script" | "style" | "title" | "textarea" ->
                  scanner.raw_text <- !p;
                  p.contents <- p.contents - 1;
                  pe := !p + 1
              | name ->
                  emit scanner (Start (make_tag name (attributes !attrs)));
                  pause ()
              end;
              begin
                mark := !p
              end;
              ()
          | 17 ->
              begin match !tag with
              | "" -> ()
              | "script" | "style" | "title" | "textarea" ->
                  scanner.raw_text <- !p;
                  p.contents <- p.contents - 1;
                  pe := !p + 1
              | name ->
                  emit scanner
                    (Start
                       (make_tag ~self_closing:true name (attributes !attrs)));
                  pause ()
              end;
              begin
                mark := !p
              end;
              ()
          | 7 ->
              begin
                tag := ""
              end;
              begin
                mark := !p
              end;
              ()
          | 6 ->
              begin
                tag := ""
              end;
              begin
                scanner.declaration <- !p;
                pe := !p + 1
              end;
              ()
          | 20 ->
              begin
                mark_end := !p
              end;
              begin
                attrs := (!key, if !mark < 0 then "" else sub ()) :: !attrs
              end;
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | 23 ->
              begin match !tag with
              | "" -> ()
              | "script" | "style" | "title" | "textarea" ->
                  scanner.raw_text <- !p;
                  p.contents <- p.contents - 1;
                  pe := !p + 1
              | name ->
                  emit scanner (Start (make_tag name (attributes !attrs)));
                  pause ()
              end;
              begin
                mark := !p
              end;
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | 18 ->
              begin match !tag with
              | "" -> ()
              | "script" | "style" | "title" | "textarea" ->
                  scanner.raw_text <- !p;
                  p.contents <- p.contents - 1;
                  pe := !p + 1
              | name ->
                  emit scanner
                    (Start
                       (make_tag ~self_closing:true name (attributes !attrs)));
                  pause ()
              end;
              begin
                mark := !p
              end;
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | _ -> ()
        with Goto_again_htmlstream -> ()
        end;

        do_again ()
      and do_again () =
        p.contents <- p.contents + 1;
        if p.contents <> pe.contents then do_resume () else do_test_eof ()
      and do_test_eof () =
        if p.contents = eof.contents then
          begin try
            begin match _htmlstream_eof_actions.(cs.contents) with
            | 3 ->
                begin
                  emit_text scanner (decode (sub ()));
                  pause ()
                end;
                ()
            | 21 ->
                begin match !tag with
                | "" -> ()
                | "script" | "style" | "title" | "textarea" ->
                    scanner.raw_text <- !p;
                    p.contents <- p.contents - 1;
                    pe := !p + 1
                | name ->
                    emit scanner (Start (make_tag name (attributes !attrs)));
                    pause ()
                end;
                ()
            | 16 ->
                begin match !tag with
                | "" -> ()
                | "script" | "style" | "title" | "textarea" ->
                    scanner.raw_text <- !p;
                    p.contents <- p.contents - 1;
                    pe := !p + 1
                | name ->
                    emit scanner
                      (Start
                         (make_tag ~self_closing:true name (attributes !attrs)));
                    pause ()
                end;
                ()
            | 5 ->
                begin
                  p.contents <- p.contents - 1;
                  begin
                    cs.contents <- 21;
                    if true then raise_notrace Goto_again_htmlstream
                  end
                end;
                ()
            | _ -> ()
            end
          with
          | Goto_again_htmlstream -> do_again ()
          | Goto_eof_trans_htmlstream -> do_eof_trans ()
          end
      in
      do_start ()
    end;

    if scanner.declaration >= 0 || scanner.raw_text >= 0 then ()
    else if !p >= !eof then scanner.finished <- true
    else if scanner.write = 0 then scanner.finished <- true
  end

let rec next scanner (_state : Html_tokenizer.state) (location : location_out) =
  if scanner.read < scanner.write then begin
    let index = scanner.read in
    let token = scanner.tokens.(index) in
    location.line <- scanner.lines.(index);
    location.column <- -1;
    scanner.tokens.(index) <- EOF;
    scanner.read <- index + 1;
    token
  end
  else if scanner.finished then begin
    location.line <- scanner.line;
    location.column <- -1;
    EOF
  end
  else begin
    scanner.read <- 0;
    scanner.write <- 0;
    run scanner;
    next scanner _state location
  end
