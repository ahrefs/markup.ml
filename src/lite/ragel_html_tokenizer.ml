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
  mutable declaration : int;
  mutable tag_scan : int;
  mutable line : int;
  tokens : Html_tokenizer.token array;
  lines : int array;
  mutable read : int;
  mutable write : int;
  mutable finished : bool;
}

let decode = Html_entity_decoder.decode

(* The first occurrence of a name wins, like src/baseline. *)
let attributes attrs =
  let rec dedupe seen = function
    | [] -> []
    | (name, value) :: rest ->
        if List.mem name seen then dedupe seen rest
        else
          (name, Html_entity_decoder.decode_attribute value)
          :: dedupe (name :: seen) rest
  in
  dedupe [] attrs

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
        9;
        62;
        0;
        122;
        10;
        62;
        9;
        62;
        10;
        62;
        10;
        10;
        0;
      |];
    ]

let _htmlstream_key_spans : int array =
  Array.concat [ [| 51; 51; 123; 1; 54; 123; 53; 54; 53; 1 |] ]

let _htmlstream_index_offsets : int array =
  Array.concat [ [| 0; 52; 104; 228; 230; 285; 409; 463; 518; 572 |] ]

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
        15;
        13;
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
        13;
        13;
        13;
        13;
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
        14;
        13;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        17;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        10;
        16;
        16;
        16;
        16;
        16;
        16;
        16;
        16;
        16;
        16;
        16;
        16;
        18;
        18;
        16;
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
        16;
        16;
        16;
        19;
        16;
        16;
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
        18;
        18;
        18;
        18;
        16;
        16;
        16;
        16;
        18;
        16;
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
        18;
        18;
        18;
        18;
        16;
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
        22;
        20;
        24;
        25;
        23;
        24;
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
        26;
        23;
        28;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        27;
        29;
        27;
        31;
        30;
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
        5;
        3;
        3;
        4;
        1;
        1;
        6;
        5;
        7;
        0;
        6;
        6;
        0;
        7;
        6;
        6;
        0;
        8;
        8;
        9;
        9;
        9;
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
        1;
        0;
        0;
        4;
        0;
        8;
        9;
        10;
        4;
        1;
        10;
        0;
        4;
        0;
        0;
        11;
        12;
        11;
        0;
        4;
        13;
        0;
        4;
      |];
    ]

let _htmlstream_eof_actions : int array =
  Array.concat [ [| 0; 3; 5; 0; 7; 5; 5; 5; 0; 0 |] ]

let htmlstream_start : int = 0
let htmlstream_first_final : int = 0
let htmlstream_error : int = -1
let htmlstream_en_garbage_tag : int = 8
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
    declaration = -1;
    tag_scan = -1;
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
  if scanner.tag_scan >= 0 then begin
    let start = scanner.tag_scan in
    scanner.tag_scan <- -1;
    let name = !tag in
    let result = Tag_attributes.scan data start in
    let next =
      if not result.Tag_attributes.ok then !eof
      else begin
        let attrs = attributes result.Tag_attributes.attributes in
        let self_closing = result.Tag_attributes.self_closing in
        match name with
        | "script" | "style" | "title" | "textarea" ->
            let after_tag = result.Tag_attributes.next in
            let body = Raw_text.scan data after_tag name in
            emit scanner (Start (make_tag ~self_closing name attrs));
            emit scanner (String body.Raw_text.text);
            if body.Raw_text.had_end_tag then
              emit scanner (End (make_tag name []));
            body.Raw_text.next
        | _ ->
            emit scanner (Start (make_tag ~self_closing name attrs));
            result.Tag_attributes.next
      end
    in
    for index = start to next - 1 do
      if data.[index] = '\n' then scanner.line <- scanner.line + 1
    done;
    p := next;
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
          | 11 ->
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
          | 13 ->
              begin
                begin
                  cs.contents <- 0;
                  if true then raise_notrace Goto_again_htmlstream
                end
              end;
              ()
          | 6 ->
              begin
                scanner.declaration <- !p;
                pe := !p + 1
              end;
              ()
          | 5 ->
              begin
                p.contents <- p.contents - 1;
                begin
                  cs.contents <- 8;
                  if true then raise_notrace Goto_again_htmlstream
                end
              end;
              ()
          | 4 ->
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | 10 ->
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
          | 12 ->
              begin
                let name = String.lowercase_ascii @@ sub () in
                emit scanner (End (make_tag name []));
                pause ()
              end;
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | 8 ->
              begin
                tag := String.lowercase_ascii @@ sub ();
                scanner.tag_scan <- !p;
                p.contents <- p.contents - 1;
                pe := !p + 1
              end;
              begin
                mark := !p
              end;
              ()
          | 9 ->
              begin
                tag := String.lowercase_ascii @@ sub ();
                scanner.tag_scan <- !p;
                p.contents <- p.contents - 1;
                pe := !p + 1
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
            | 7 ->
                begin
                  tag := String.lowercase_ascii @@ sub ();
                  scanner.tag_scan <- !p;
                  p.contents <- p.contents - 1;
                  pe := !p + 1
                end;
                ()
            | 5 ->
                begin
                  p.contents <- p.contents - 1;
                  begin
                    cs.contents <- 8;
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

    if scanner.declaration >= 0 || scanner.tag_scan >= 0 then ()
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
