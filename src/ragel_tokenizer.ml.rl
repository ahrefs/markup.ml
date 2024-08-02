[@@@ocaml.warning "-38-32"]
open Common
open Token_tag

let id = Fun.id

module Fresh(T : sig type t val compare : t -> t -> int end)() =
struct
  type t = T.t
  let inject = id
  let project = id
  let inject_list = id
  let project_list = id
  let compare = T.compare
  let equal a b = T.compare a b = 0
end

module Raw = struct
  include Fresh(String)()
  let length x = String.length @@ project x
  let is_empty x = "" = project x
end

type ctx = { mutable lnum : int }

let get_lnum ctx = ctx.lnum

let init () = { lnum = 1 }

%%{
 machine htmlstream;

 action mark { mark := !p }
 action mark_end { mark_end := !p }
 action tag { tag := String.lowercase_ascii @@ sub (); attrs := []; }
 action close_tag { call @@ `End {name =  (String.lowercase_ascii @@ sub ()); attributes = []; self_closing = false}  }
 action directive { directive := String.lowercase_ascii @@ sub (); attrs := []; }
 action text { call @@ `String (Raw.inject @@ sub ()) }
 action key { key := String.lowercase_ascii @@ sub () }
 action store_attr { attrs := (!key, Raw.inject (if !mark < 0 then "" else sub())) :: !attrs }
 action tag_done {
    match !tag with
    | "script" -> fhold; fgoto in_script;
    | "style" -> fhold; fgoto in_style;
    | "" -> ()
    | _ -> call @@ `Start {name = !tag; attributes = List.rev !attrs; self_closing = false}
 }
 action tag_done_2 { call @@ `Start {name = !tag; attributes = List.rev !attrs; self_closing = true} }
 action directive_done { (* printfn "directive %s" !directive; *) }

 action garbage_tag { (*printfn "GARBAGE %S" (current ()); *) fhold; fgoto garbage_tag;}

 count_newlines = ('\n' >{ ctx.lnum <- ctx.lnum + 1 } | ^'\n'+)**;

 wsp = 0..32;
 ident = alnum | '-' | [_:] ;

 in_script := (count_newlines | any* >mark %mark_end :>> ('<' wsp* '/' wsp* 'script'i wsp* '>' >{} @{fgoto main;}));
 in_style := (count_newlines | any* >mark %mark_end :>> ('<' wsp* '/' wsp* 'style'i wsp* '>' >{} @{fgoto main;}));

 garbage_tag := (count_newlines | ^'>'* '>' @tag_done @{ fgoto main; });

 literal = ( "'" ^"'"* >mark %mark_end "'" | '"' ^'"'* >mark %mark_end '"' | ^(wsp|'"'|"'"|'>')+ >mark %mark_end);
 tag_attrs = (wsp+ | ident+ >mark %key wsp* ('=' wsp* literal)? %store_attr )**;
 close_tag = '/' wsp* ident* >mark %close_tag <: ^'>'* '>';
 open_tag = ident+ >mark %tag <: wsp* tag_attrs ('/' wsp* '>' %tag_done_2 | '>' %tag_done);
 directive = ('!'|'?') (alnum ident+) >mark %directive <: wsp* tag_attrs '?'? '>' %directive_done;
 comment = "!--" any* :>> "-->";
 # reset tag so that garbage_tag will not generate duplicate tag with tag_done
 tag = '<' wsp* <: (close_tag | open_tag | directive | comment) @lerr(garbage_tag) >{ tag := "" };
 main := (((tag | ^'<' >mark ^'<'* %text ) )** | count_newlines);

 write data;
}%%

(** scan [data] for html tags and invoke [call] for every element  *)
let parse ?(ctx=init ()) call data =
  let cs = ref 0 in
  let mark = ref (-1) in
  let mark_end = ref (-1) in
  let tag = ref "" and key = ref "" and attrs = ref [] and directive = ref "" in
(*  let substr data ofs len = try String.sub data ofs len with exn -> Prelude.printfn "%S %d %d %d" data (String.length data) ofs len; raise exn in *)
  let substr = String.sub in
  %%write init;
  let eof = ref (String.length data) in
  let p = ref 0 in
  let pe = ref (String.length data) in
  let sub () =
    assert (!mark >= 0);
    if !mark_end < 0 then mark_end := !p;
    let s = if !mark_end <= !mark then "" else substr data !mark (!mark_end - !mark) in
    mark := -1;
    mark_end := -1;
    s
  in
  let call element = 
    call ((get_lnum ctx, !mark_end), element)
  in
  %%write exec;

  call `EOF;
(* FIXME ? *)
(*     if !eof <> -1 && !cs < htmlstream_first_final then Exn.fail "not parsed"; *)
  ()

(* vim: ft=ocaml
*)