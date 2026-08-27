(* Derived from Devkit htmlStream_ragel.ml.rl.
   Devkit is distributed under LGPL-2.1-only with the OCaml linking exception.
   The original source is available from https://github.com/ygrek/ocaml-webstack. *)

[@@@ocaml.warning "-38-32"]

open Common
open Html_tokenizer

type location_out = {
  mutable line : int;
  mutable column : int;
}

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
  {Token_tag.name; attributes; self_closing}

let buffer_capacity = 128
let maximum_transition_output = 3

let emit scanner token =
  scanner.tokens.(scanner.write) <- token;
  scanner.lines.(scanner.write) <- scanner.line;
  scanner.write <- scanner.write + 1

let emit_many scanner tokens =
  List.iter (emit scanner) tokens

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

%%{
 machine htmlstream;

 action mark { mark := !p }
 action mark_end { mark_end := !p }
 action close_tag {
   let name = String.lowercase_ascii @@ sub () in
   emit scanner (End (make_tag name []));
   pause ();
 }
 action text {
   emit_text scanner (decode (sub ()));
   pause ();
 }
 action tag_start {
   tag := String.lowercase_ascii @@ sub ();
   scanner.tag_scan <- !p;
   fhold;
   pe := !p + 1;
 }
 action garbage_tag_done { fgoto main; }
 action markup_declaration { scanner.declaration <- !p; pe := !p + 1; }

 action garbage_tag { fhold; fgoto garbage_tag; }

 count_newlines = ('\n' >{ scanner.line <- scanner.line + 1 } | ^'\n'+)**;

 wsp = 0..32;
 html_ws = 0x09 | 0x0A | 0x0C | 0x0D | 0x20;
 ident = alnum | '-' | [_:.] ;
 tag_name = ident ( any - ( html_ws | '/' | '>' ) )*;

 garbage_tag := (count_newlines | ^'>'* '>' @garbage_tag_done);

 close_tag = '/' wsp* tag_name? >mark %close_tag <: ^'>'* '>';
 open_tag = tag_name >mark %tag_start;
 declaration = ('!'|'?') @markup_declaration;
 tag = '<' wsp* <:
   (close_tag | open_tag | declaration)
   @lerr(garbage_tag);
 main := (((tag | ^'<' >mark ^'<'* %text ) )** | count_newlines);

 write data;
}%%

let create data =
  let cs = ref 0 in
  %%write init;
  let length = String.length data in
  {data;
   cs;
   p = ref 0;
   pe = ref length;
   eof = ref length;
   mark = ref (-1);
   mark_end = ref (-1);
   tag = ref "";
   declaration = (-1);
   tag_scan = (-1);
   line = 1;
   tokens = Array.make buffer_capacity EOF;
   lines = Array.make buffer_capacity 1;
   read = 0;
   write = 0;
   finished = false}

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
    if scanner.write >= buffer_capacity - maximum_transition_output &&
        !p < !eof then
      pe := !p + 1
  in
  let substr = String.sub in
  let sub () =
    assert (!mark >= 0);
    if !mark_end < 0 then mark_end := !p;
    let text =
      if !mark_end <= !mark then ""
      else substr data !mark (!mark_end - !mark)
    in
    mark := -1;
    mark_end := -1;
    text
  in
  if scanner.tag_scan >= 0 then begin
    let start = scanner.tag_scan in
    scanner.tag_scan <- (-1);
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
    scanner.declaration <- (-1);
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
    %%write exec;
    if scanner.declaration >= 0 || scanner.tag_scan >= 0 then ()
    else if !p >= !eof then scanner.finished <- true
    else if scanner.write = 0 then scanner.finished <- true
  end

let rec next scanner (_state : Html_tokenizer.state)
    (location : location_out) =
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
