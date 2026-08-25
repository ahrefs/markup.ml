(* Derived from Devkit's htmlStream_ragel.ml.rl.
   Devkit is distributed under LGPL-2.1-only with the OCaml linking exception.
   The original source is available from https://github.com/ygrek/ocaml-webstack. *)

[@@@ocaml.warning "-38-32"]

open Common

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
  directive : string ref;
  mutable line : int;
  mutable pending : (int * Html_tokenizer.token) list;
  mutable finished : bool;
}

let decode text =
  try Devkit.Web.htmldecode text with _ -> text

let attributes attrs =
  List.map (fun (name, value) -> name, decode value) attrs

let make_tag name attributes =
  {Token_tag.name; attributes; self_closing = false}

let emit scanner token =
  scanner.pending <- scanner.pending @ [scanner.line, token]

let emit_many scanner tokens =
  scanner.pending <-
    scanner.pending @ List.map (fun token -> scanner.line, token) tokens

%%{
 machine htmlstream;

 action mark { mark := !p }
 action mark_end { mark_end := !p }
 action tag { tag := String.lowercase_ascii @@ sub (); attrs := []; }
 action close_tag {
   let name = String.lowercase_ascii @@ sub () in
   if name <> "br" then begin
     emit scanner (`End (make_tag name []));
     pause ()
   end;
 }
 action directive { directive := String.lowercase_ascii @@ sub (); attrs := []; }
 action text {
   emit scanner (`String (decode (sub ())));
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
     emit scanner (`Start (make_tag name (attributes !attrs)));
     pause ();
 }
 action tag_done_2 {
   let start = `Start (make_tag !tag (attributes !attrs)) in
   if !tag = "a" || !tag = "br" then emit scanner start
   else emit_many scanner [start; `End (make_tag !tag [])];
   pause ();
 }
 action directive_done { }

 action garbage_tag { fhold; fgoto garbage_tag; }

 count_newlines = ('\n' >{ scanner.line <- scanner.line + 1 } | ^'\n'+)**;

 wsp = 0..32;
 ident = alnum | '-' | [_:.] ;

 in_script :=
   (count_newlines | any* >mark %mark_end :>>
     ('<' wsp* '/' wsp* 'script'i wsp* '>' >{
       emit_many scanner
         [`Start (make_tag "script" (attributes !attrs));
          `String (sub ());
          `End (make_tag "script" [])];
       pause ();
     } @{fgoto main;}));
 in_style :=
   (count_newlines | any* >mark %mark_end :>>
     ('<' wsp* '/' wsp* 'style'i wsp* '>' >{
       emit_many scanner
         [`Start (make_tag "style" (attributes !attrs));
          `String (sub ());
          `End (make_tag "style" [])];
       pause ();
     } @{fgoto main;}));
 in_title :=
   (count_newlines | any* >mark %mark_end :>>
     ('<' wsp* '/' wsp* 'title'i wsp* '>' >{
       emit_many scanner
         [`Start (make_tag "title" (attributes !attrs));
          `String (decode (sub ()));
          `End (make_tag "title" [])];
       pause ();
     } @{fgoto main;}));

 garbage_tag := (count_newlines | ^'>'* '>' @tag_done @{ fgoto main; });

 literal =
   ("'" ^"'"* >mark %mark_end "'" |
    '"' ^'"'* >mark %mark_end '"' |
    ^(wsp|'"'|"'"|'>')+ >mark %mark_end);
 tag_attrs = (wsp+ | ident+ >mark %key wsp* ('=' wsp* literal)? %store_attr )**;
 close_tag = '/' wsp* ident* >mark %close_tag <: ^'>'* '>';
 open_tag = ident+ >mark %tag <: wsp* tag_attrs
   ('/' wsp* '>' %tag_done_2 | '>' %tag_done);
 directive = ('!'|'?') (alnum ident+) >mark %directive <:
   wsp* tag_attrs '?'? '>' %directive_done;
 comment = "!--" any* :>> "-->";
 tag = '<' wsp* <:
   (close_tag | open_tag | directive | comment)
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
   directive = ref "";
   line = 1;
   pending = [];
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
  let directive = scanner.directive in
  pe := !eof;
  let pause () = if !p < !eof then pe := !p + 1 in
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
  %%write exec;
  if !p >= !eof then scanner.finished <- true
  else if scanner.pending = [] then scanner.finished <- true

let rec next scanner (_state : Html_tokenizer.state)
    (location : location_out) =
  match scanner.pending with
  | (line, token)::rest ->
    scanner.pending <- rest;
    location.line <- line;
    location.column <- -1;
    token
  | [] when scanner.finished ->
    location.line <- scanner.line;
    location.column <- -1;
    `EOF
  | [] ->
    run scanner;
    next scanner _state location
