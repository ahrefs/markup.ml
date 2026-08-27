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
  key : string ref;
  attrs : (string * string) list ref;
  mutable declaration : int;
  mutable line : int;
  tokens : Html_tokenizer.token array;
  lines : int array;
  mutable read : int;
  mutable write : int;
  mutable finished : bool;
}

let decode = Html_entity_decoder.decode

let attributes attrs =
  List.map
    (fun (name, value) -> name, Html_entity_decoder.decode_attribute value)
    attrs

let make_tag name attributes =
  {Token_tag.name; attributes; self_closing = false}

let buffer_capacity = 128
let maximum_transition_output = 3

let emit scanner token =
  scanner.tokens.(scanner.write) <- token;
  scanner.lines.(scanner.write) <- scanner.line;
  scanner.write <- scanner.write + 1

let emit_many scanner tokens =
  List.iter (emit scanner) tokens

%%{
 machine htmlstream;

 action mark { mark := !p }
 action mark_end { mark_end := !p }
 action tag { tag := String.lowercase_ascii @@ sub (); attrs := []; }
 action close_tag {
   let name = String.lowercase_ascii @@ sub () in
   emit scanner (End (make_tag name []));
   pause ();
 }
 action text {
   emit scanner (String (decode (sub ())));
   pause ();
 }
 action key { key := String.lowercase_ascii @@ sub () }
 action store_attr { attrs := (!key, if !mark < 0 then "" else sub()) :: !attrs }
 action tag_done {
   match !tag with
   | "script" -> fhold; fgoto in_script;
   | "style" -> fhold; fgoto in_style;
   | "title" -> fhold; fgoto in_title;
   | "" -> ()
   | name ->
     emit scanner (Start (make_tag name (attributes !attrs)));
     pause ();
 }
 action tag_done_2 {
   let start = Start (make_tag !tag (attributes !attrs)) in
   if !tag = "a" || !tag = "br" then emit scanner start
   else emit_many scanner [start; End (make_tag !tag [])];
   pause ();
 }
 action garbage_tag_done {
   match !tag with
   | "script" -> fhold; fgoto in_script;
   | "style" -> fhold; fgoto in_style;
   | "title" -> fhold; fgoto in_title;
   | "" -> fgoto main;
   | name ->
     emit scanner (Start (make_tag name (attributes !attrs)));
     pause ();
     fgoto main;
 }
 action markup_declaration { scanner.declaration <- !p; pe := !p + 1; }

 action garbage_tag { fhold; fgoto garbage_tag; }

 count_newlines = ('\n' >{ scanner.line <- scanner.line + 1 } | ^'\n'+)**;

 wsp = 0..32;
 ident = alnum | '-' | [_:.] ;
 tag_name = ident ( any - ( wsp | '/' | '>' ) )*;

 in_script :=
   (count_newlines | any* >mark %mark_end :>>
     ('<' wsp* '/' wsp* 'script'i wsp* '>' >{
       emit_many scanner
         [Start (make_tag "script" (attributes !attrs));
          String (sub ());
          End (make_tag "script" [])];
       pause ();
     } @{fgoto main;}));
 in_style :=
   (count_newlines | any* >mark %mark_end :>>
     ('<' wsp* '/' wsp* 'style'i wsp* '>' >{
       emit_many scanner
         [Start (make_tag "style" (attributes !attrs));
          String (sub ());
          End (make_tag "style" [])];
       pause ();
     } @{fgoto main;}));
 in_title :=
   (count_newlines | any* >mark %mark_end :>>
     ('<' wsp* '/' wsp* 'title'i wsp* '>' >{
       emit_many scanner
         [Start (make_tag "title" (attributes !attrs));
          String (decode (sub ()));
          End (make_tag "title" [])];
       pause ();
     } @{fgoto main;}));

 garbage_tag := (count_newlines | ^'>'* '>' @garbage_tag_done);

 literal =
   ("'" ^"'"* >mark %mark_end "'" |
    '"' ^'"'* >mark %mark_end '"' |
    ^(wsp|'"'|"'"|'>')+ >mark %mark_end);
 tag_attrs = (wsp+ | ident+ >mark %key wsp* ('=' wsp* literal)? %store_attr )**;
 close_tag = '/' wsp* tag_name? >mark %close_tag <: ^'>'* '>';
 open_tag = tag_name >mark %tag <: wsp* tag_attrs
   ('/' wsp* '>' %tag_done_2 | '>' %tag_done);
 declaration = ('!'|'?') @markup_declaration;
 tag = '<' wsp* <:
   (close_tag | open_tag | declaration)
   @lerr(garbage_tag) >{ tag := "" };
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
   key = ref "";
   attrs = ref [];
   declaration = (-1);
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
  let key = scanner.key in
  let attrs = scanner.attrs in
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
  if scanner.declaration >= 0 then begin
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
    if scanner.declaration >= 0 then ()
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
