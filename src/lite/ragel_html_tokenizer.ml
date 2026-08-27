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
  tag : string ref;
  mutable last_start_tag : string;
  mutable declaration : int;
  mutable bogus : int;
  mutable tag_scan : int;
  mutable end_scan : int;
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

let normalize_name text =
  let text = String.lowercase_ascii text in
  if not (String.contains text '\x00') then text
  else begin
    let buffer = Buffer.create (String.length text + 8) in
    String.iter
      (fun byte ->
        if byte = '\x00' then Buffer.add_string buffer "\xEF\xBF\xBD"
        else Buffer.add_char buffer byte)
      text;
    Buffer.contents buffer
  end

let buffer_capacity = 128
let maximum_transition_output = 3

let emit scanner token =
  scanner.tokens.(scanner.write) <- token;
  scanner.lines.(scanner.write) <- scanner.line;
  scanner.write <- scanner.write + 1

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
  if boundary = 0 || boundary = length then emit scanner (`String text)
  else begin
    emit scanner (`String (String.sub text 0 boundary));
    emit scanner (`String (String.sub text boundary (length - boundary)))
  end

let _htmlstream_trans_keys : int array =
  Array.concat [ [| 10; 60; 10; 60; 10; 122; 10; 122; 9; 62; 9; 62; 0 |] ]

let _htmlstream_key_spans : int array =
  Array.concat [ [| 51; 51; 113; 113; 54; 54 |] ]

let _htmlstream_index_offsets : int array =
  Array.concat [ [| 0; 52; 104; 218; 332; 387 |] ]

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
        7;
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
        6;
        6;
        9;
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
        8;
        6;
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
        10;
        10;
        10;
        10;
        6;
        6;
        6;
        6;
        6;
        6;
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
        10;
        10;
        10;
        10;
        6;
        12;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        11;
        13;
        11;
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
        11;
        11;
        11;
        11;
        11;
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
        11;
        16;
        17;
        15;
        16;
        16;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        16;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        16;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        15;
        16;
        15;
        19;
        20;
        18;
        19;
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
        19;
        18;
        0;
      |];
    ]

let _htmlstream_trans_targs : int array =
  Array.concat
    [ [| 1; 1; 2; 1; 1; 2; 0; 0; 0; 3; 5; 0; 0; 0; 4; 4; 1; 1; 5; 1; 1 |] ]

let _htmlstream_trans_actions : int array =
  Array.concat
    [
      [| 1; 2; 0; 0; 4; 3; 6; 7; 8; 0; 1; 10; 11; 0; 1; 0; 13; 14; 0; 16; 17 |];
    ]

let _htmlstream_eof_actions : int array =
  Array.concat [ [| 0; 3; 5; 9; 12; 15 |] ]

let htmlstream_start : int = 0
let htmlstream_first_final : int = 0
let htmlstream_error : int = -1
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
    tag = ref "";
    last_start_tag = "";
    declaration = -1;
    bogus = -1;
    tag_scan = -1;
    end_scan = -1;
    line = 1;
    tokens = Array.make buffer_capacity `EOF;
    lines = Array.make buffer_capacity 1;
    read = 0;
    write = 0;
    finished = false;
  }

let run scanner foreign =
  let data = scanner.data in
  let cs = scanner.cs in
  let p = scanner.p in
  let pe = scanner.pe in
  let eof = scanner.eof in
  let mark = scanner.mark in
  let tag = scanner.tag in
  pe := !eof;
  let pause () =
    if scanner.write >= buffer_capacity - maximum_transition_output && !p < !eof
    then pe := !p + 1
  in
  let sub () =
    assert (!mark >= 0);
    let text = if !p <= !mark then "" else String.sub data !mark (!p - !mark) in
    mark := -1;
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
        scanner.last_start_tag <- name;
        emit scanner (`Start (make_tag ~self_closing name attrs));
        result.Tag_attributes.next
      end
    in
    for index = start + 1 to next - 1 do
      if data.[index] = '\n' then scanner.line <- scanner.line + 1
    done;
    p := next;
    cs := htmlstream_en_main;
    if !p >= !eof then scanner.finished <- true
  end
  else if scanner.end_scan >= 0 then begin
    let start = scanner.end_scan in
    scanner.end_scan <- -1;
    let name = !tag in
    let result = Tag_attributes.scan data start in
    if result.Tag_attributes.ok then emit scanner (`End (make_tag name []));
    let next =
      if result.Tag_attributes.ok then result.Tag_attributes.next else !eof
    in
    for index = start + 1 to next - 1 do
      if data.[index] = '\n' then scanner.line <- scanner.line + 1
    done;
    p := next;
    cs := htmlstream_en_main;
    if !p >= !eof then scanner.finished <- true
  end
  else if scanner.bogus >= 0 then begin
    let start = scanner.bogus in
    scanner.bogus <- -1;
    (* The consumed character is a codepoint, not a byte. *)
    let width =
      if data.[start] < '\x80' then 1
      else if data.[start] < '\xE0' then 2
      else if data.[start] < '\xF0' then 3
      else 4
    in
    let start = min (start + width) !eof in
    let result = Markup_declaration.bogus_comment data start in
    emit scanner result.Markup_declaration.token;
    for index = start to result.Markup_declaration.next - 1 do
      if data.[index] = '\n' then scanner.line <- scanner.line + 1
    done;
    p := result.Markup_declaration.next;
    cs := htmlstream_en_main;
    if !p >= !eof then scanner.finished <- true
  end
  else if scanner.declaration >= 0 then begin
    let start = scanner.declaration in
    scanner.declaration <- -1;
    let result = Markup_declaration.scan ~foreign data start in
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
          | 3 ->
              begin
                emit_text scanner (decode (sub ()));
                pause ()
              end;
              ()
          | 8 ->
              begin
                scanner.declaration <- !p;
                pe := !p + 1
              end;
              ()
          | 10 ->
              begin
                scanner.bogus <- !p;
                pe := !p + 1
              end;
              ()
          | 6 ->
              begin
                emit scanner (`String "<");
                pause ();
                p.contents <- p.contents - 1;
                begin
                  cs.contents <- 0;
                  if true then raise_notrace Goto_again_htmlstream
                end
              end;
              ()
          | 4 ->
              begin
                scanner.line <- scanner.line + 1
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
          | 13 ->
              begin
                tag := normalize_name @@ sub ();
                scanner.end_scan <- !p;
                p.contents <- p.contents - 1;
                pe := !p + 1
              end;
              begin
                mark := !p
              end;
              ()
          | 16 ->
              begin
                tag := normalize_name @@ sub ();
                scanner.tag_scan <- !p;
                p.contents <- p.contents - 1;
                pe := !p + 1
              end;
              begin
                mark := !p
              end;
              ()
          | 11 ->
              begin
                scanner.bogus <- !p;
                pe := !p + 1
              end;
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | 7 ->
              begin
                emit scanner (`String "<");
                pause ();
                p.contents <- p.contents - 1;
                begin
                  cs.contents <- 0;
                  if true then raise_notrace Goto_again_htmlstream
                end
              end;
              begin
                scanner.line <- scanner.line + 1
              end;
              ()
          | 14 ->
              begin
                tag := normalize_name @@ sub ();
                scanner.end_scan <- !p;
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
          | 17 ->
              begin
                tag := normalize_name @@ sub ();
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
            | 12 ->
                begin
                  tag := normalize_name @@ sub ();
                  scanner.end_scan <- !p;
                  p.contents <- p.contents - 1;
                  pe := !p + 1
                end;
                ()
            | 3 ->
                begin
                  emit_text scanner (decode (sub ()));
                  pause ()
                end;
                ()
            | 15 ->
                begin
                  tag := normalize_name @@ sub ();
                  scanner.tag_scan <- !p;
                  p.contents <- p.contents - 1;
                  pe := !p + 1
                end;
                ()
            | 5 ->
                begin
                  emit scanner (`String "<")
                end;
                ()
            | 9 ->
                begin
                  emit scanner (`String "<");
                  emit scanner (`String "/")
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

    if
      scanner.declaration >= 0 || scanner.bogus >= 0 || scanner.tag_scan >= 0
      || scanner.end_scan >= 0
    then ()
    else if !p >= !eof then scanner.finished <- true
    else if scanner.write = 0 then scanner.finished <- true
  end

(* The tree builder requested a non-Data state for the next scan; the last
   start tag emitted is the appropriate end tag. In fragment parsing no start
   tag has been seen, so no end tag ever matches. *)
let scan_raw_state scanner state foreign =
  let data = scanner.data in
  let start = !(scanner.p) in
  if start >= !(scanner.eof) then scanner.finished <- true
  else begin
    let next_index =
      match (state : Html_tokenizer.state) with
      | PLAINTEXT ->
          let body = Raw_text.plaintext data start in
          emit scanner (`String body.Raw_text.text);
          body.Raw_text.next
      | _ ->
          let name = scanner.last_start_tag in
          if name = "" then begin
            let body = Raw_text.plaintext data start in
            let text =
              match (state : Html_tokenizer.state) with
              | RCDATA -> decode body.Raw_text.text
              | _ -> body.Raw_text.text
            in
            emit scanner (`String text);
            body.Raw_text.next
          end
          else begin
            let body =
              Raw_text.scan ~drop_end_tag_candidate:foreign data start name
            in
            emit scanner (`String body.Raw_text.text);
            if body.Raw_text.had_end_tag then
              emit scanner (`End (make_tag name []));
            body.Raw_text.next
          end
    in
    for index = start to next_index - 1 do
      if data.[index] = '\n' then scanner.line <- scanner.line + 1
    done;
    scanner.p := next_index;
    scanner.cs := htmlstream_en_main;
    if next_index >= !(scanner.eof) then scanner.finished <- true
  end

let rec next scanner (state : Html_tokenizer.state) foreign
    (location : location_out) =
  if scanner.read < scanner.write then begin
    let index = scanner.read in
    let token = scanner.tokens.(index) in
    location.line <- scanner.lines.(index);
    location.column <- -1;
    scanner.tokens.(index) <- `EOF;
    scanner.read <- index + 1;
    token
  end
  else if scanner.finished then begin
    location.line <- scanner.line;
    location.column <- -1;
    `EOF
  end
  else if state <> Data then begin
    scanner.read <- 0;
    scanner.write <- 0;
    scan_raw_state scanner state foreign;
    next scanner Data foreign location
  end
  else begin
    scanner.read <- 0;
    scanner.write <- 0;
    run scanner foreign;
    next scanner state foreign location
  end
