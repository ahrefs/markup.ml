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
{Token_tag.name; attributes; self_closing}

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
if boundary = 0 || boundary = length then emit scanner (String text)
else begin
	emit scanner (String (String.sub text 0 boundary));
	emit scanner (String (String.sub text boundary (length - boundary)))
end

let _htmlstream_trans_keys : int array = [|
1; 5; 1; 5; 1; 7; 1; 7; 0; 6; 0; 6; 0 ;
|]
let _htmlstream_char_class : int array = [|
0; 1; 2; 0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 0; 3; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 4; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 5; 2; 6; 3; 2; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 2; 2; 2; 2; 2; 2; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 0 ;
|]
let _htmlstream_index_offsets : int array = [|
0; 5; 10; 17; 24; 31; 0 ;
|]
let _htmlstream_indices : int array = [|
2; 1; 1; 1; 3; 6; 5; 5; 5; 7; 10; 9; 11; 12; 9; 9; 13; 16; 15; 15; 15; 15; 0; 17; 20; 21; 19; 19; 20; 19; 20; 24; 25; 23; 23; 24; 23; 24; 0 ;
|]
let _htmlstream_index_defaults : int array = [|
1; 5; 9; 15; 19; 23; 0 ;
|]
let _htmlstream_cond_targs : int array = [|
0; 1; 1; 2; 1; 1; 1; 2; 2; 0; 0; 0; 3; 5; 3; 0; 0; 4; 4; 4; 1; 1; 5; 5; 1; 1; 0 ;
|]
let _htmlstream_cond_actions : int array = [|
0; 1; 2; 0; 3; 0; 4; 3; 5; 6; 7; 8; 0; 1; 9; 10; 11; 1; 12; 0; 13; 14; 15; 0; 16; 17; 0 ;
|]
let _htmlstream_eof_trans : int array = [|
1; 5; 9; 15; 19; 23; 0 ;
|]
let htmlstream_start  : int  = 0
let htmlstream_first_final  : int  = 0
let htmlstream_error  : int  = -1
let htmlstream_en_main  : int  = 0
let create data =
let cs = ref 0 in
begin
	cs  := htmlstream_start;

end;
let length = String.length data in
{data;
	cs;
	p = ref 0;
	pe = ref length;
	eof = ref length;
	mark = ref (-1);
	tag = ref "";
	last_start_tag = "";
	declaration = (-1);
	bogus = (-1);
	tag_scan = (-1);
	end_scan = (-1);
	line = 1;
	tokens = Array.make buffer_capacity EOF;
	lines = Array.make buffer_capacity 1;
	read = 0;
	write = 0;
	finished = false}

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
if scanner.write >= buffer_capacity - maximum_transition_output &&
!p < !eof then
pe := !p + 1
in
let sub () =
assert (!mark >= 0);
let text =
if !p <= !mark then "" else String.sub data !mark (!p - !mark)
in
mark := -1;
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
		scanner.last_start_tag <- name;
		emit scanner (Start (make_tag ~self_closing name attrs));
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
	scanner.end_scan <- (-1);
	let name = !tag in
	let result = Tag_attributes.scan data start in
	if result.Tag_attributes.ok then
	emit scanner (End (make_tag name []));
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
	scanner.bogus <- (-1);
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
	scanner.declaration <- (-1);
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
		let _trans  : int  ref = ref ( 0 ) in
		let _keys : int ref = ref 0 in
		let _inds : int ref = ref 0 in
		let _ic : int  ref = ref 0 in
		let _have  : int  ref = ref ( 0 ) in
		let _cont  : int  ref = ref ( 1 ) in
		let _again  : int  ref = ref ( 1 ) in
		let _bsc  : int  ref = ref ( 1 ) in
		while _again.contents= 1 && ( p.contents!= pe.contents|| p.contents= eof.contents ) do
		begin
			_cont  := 1;
			_again  := 1;
			if p.contents= eof.contents then
			begin
				begin
					if _htmlstream_eof_trans.(cs.contents)> 0  then
					begin
						begin
							_trans  := _htmlstream_eof_trans.(cs.contents)- 1;

						end;

					end
					;
				end;

			end
			else
			begin
				begin
					_keys  := ( cs.contents lsl 1 );
					_inds  := _htmlstream_index_offsets.(cs.contents);
					if ( Char.code data.[p.contents] )<= 122 && ( Char.code data.[p.contents] )>= 9  then
					begin
						begin
							_ic  := _htmlstream_char_class.(( Char.code data.[p.contents] )- 9);
							if _ic.contents<= _htmlstream_trans_keys.( _keys.contents+1  )&& _ic.contents>= _htmlstream_trans_keys.( _keys.contents ) then
							begin
								_trans  := _htmlstream_indices.( _inds.contents+ ( _ic.contents- _htmlstream_trans_keys.( _keys.contents ) ) );

							end
							else
							begin
								_trans  := _htmlstream_index_defaults.(cs.contents);

							end
							;
						end;

					end
					else
					begin
						begin
							_trans  := _htmlstream_index_defaults.(cs.contents);

						end;

					end
					;
				end;

			end
			;cs  := _htmlstream_cond_targs.(_trans.contents);
			if _htmlstream_cond_actions.(_trans.contents)!= 0  then
			begin
				begin
					if _htmlstream_cond_actions.(_trans.contents) = 1  then
					begin
						begin
							mark := !p
						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 12  then
					begin
						begin
							tag := normalize_name @@ sub ();
							scanner.end_scan <- !p;
							begin
								p  := p.contents- 1;

							end;

							pe := !p + 1;

						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 3  then
					begin
						begin
							emit_text scanner (decode (sub ()));
							pause ();

						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 15  then
					begin
						begin
							tag := normalize_name @@ sub ();
							scanner.tag_scan <- !p;
							begin
								p  := p.contents- 1;

							end;

							pe := !p + 1;

						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 8  then
					begin
						begin
							scanner.declaration <- !p; pe := !p + 1;
						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 10  then
					begin
						begin
							scanner.bogus <- !p; pe := !p + 1;
						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 6  then
					begin
						begin
							emit scanner (String "<");
							pause ();
							begin
								p  := p.contents- 1;

							end;

							begin
								cs  := 0;

							end;

						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 5  then
					begin
						begin
							emit scanner (String "<")
						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 9  then
					begin
						begin
							emit scanner (String "<");
							emit scanner (String "/")

						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 4  then
					begin
						begin
							scanner.line <- scanner.line + 1
						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 2  then
					begin
						begin
							mark := !p
						end;
						begin
							scanner.line <- scanner.line + 1
						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 13  then
					begin
						begin
							tag := normalize_name @@ sub ();
							scanner.end_scan <- !p;
							begin
								p  := p.contents- 1;

							end;

							pe := !p + 1;

						end;
						begin
							mark := !p
						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 16  then
					begin
						begin
							tag := normalize_name @@ sub ();
							scanner.tag_scan <- !p;
							begin
								p  := p.contents- 1;

							end;

							pe := !p + 1;

						end;
						begin
							mark := !p
						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 11  then
					begin
						begin
							scanner.bogus <- !p; pe := !p + 1;
						end;
						begin
							scanner.line <- scanner.line + 1
						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 7  then
					begin
						begin
							emit scanner (String "<");
							pause ();
							begin
								p  := p.contents- 1;

							end;

							begin
								cs  := 0;

							end;

						end;
						begin
							scanner.line <- scanner.line + 1
						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 14  then
					begin
						begin
							tag := normalize_name @@ sub ();
							scanner.end_scan <- !p;
							begin
								p  := p.contents- 1;

							end;

							pe := !p + 1;

						end;
						begin
							mark := !p
						end;
						begin
							scanner.line <- scanner.line + 1
						end;

					end
					else if _htmlstream_cond_actions.(_trans.contents) = 17  then
					begin
						begin
							tag := normalize_name @@ sub ();
							scanner.tag_scan <- !p;
							begin
								p  := p.contents- 1;

							end;

							pe := !p + 1;

						end;
						begin
							mark := !p
						end;
						begin
							scanner.line <- scanner.line + 1
						end;

					end
					;

				end;

			end
			;if _cont.contents= 1  then
			begin
				begin
					if p.contents= eof.contents then
					begin
						begin
							if cs.contents>= 0  then
							begin
								begin
									_cont  := 0;
									_again  := 0;

								end;

							end
							;
						end;

					end
					else
					begin
						begin
							p  := p.contents + 1;
							begin
								_cont  := 0;
								_again  := 1;

							end;

						end;

					end
					;if _cont.contents= 1  then
					begin
						begin
							begin
								_cont  := 0;
								_again  := 0;

							end;

						end;

					end
					;
				end;

			end
			;
		end;

		done;

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
	emit scanner (String body.Raw_text.text);
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
		emit scanner (String text);
		body.Raw_text.next
	end
	else begin
		let body =
		Raw_text.scan ~drop_end_tag_candidate:foreign data start name
		in
		emit scanner (String body.Raw_text.text);
		if body.Raw_text.had_end_tag then
		emit scanner (End (make_tag name []));
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
	scanner.tokens.(index) <- EOF;
	scanner.read <- index + 1;
	token
end
else if scanner.finished then begin
	location.line <- scanner.line;
	location.column <- -1;
	EOF
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
