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
	tokens : Html_tokenizer.token array;
	lines : int array;
	mutable read : int;
	mutable write : int;
	mutable finished : bool;
}

let decode text =
try Devkit.Web.htmldecode text with _ -> text

let attributes attrs =
List.map (fun (name, value) -> name, decode value) attrs

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

let _htmlstream_trans_keys : int array = [|
1; 10; 1; 10; 0; 22; 1; 1; 1; 22; 1; 6; 1; 6; 1; 6; 1; 12; 1; 22; 0; 22; 0; 22; 0; 22; 0; 22; 0; 12; 0; 12; 1; 10; 1; 3; 1; 3; 0; 22; 1; 12; 1; 5; 1; 5; 0; 22; 0; 22; 0; 22; 0; 22; 0; 12; 1; 10; 0; 12; 0; 12; 1; 10; 1; 3; 1; 3; 0; 22; 1; 5; 1; 5; 0; 22; 1; 12; 1; 22; 1; 22; 1; 10; 1; 10; 0; 10; 0; 20; 1; 14; 1; 19; 1; 16; 1; 18; 1; 21; 0; 12; 1; 1; 1; 10; 1; 10; 0; 10; 0; 20; 1; 21; 1; 22; 1; 17; 1; 15; 0; 12; 1; 1; 1; 10; 1; 10; 0; 10; 0; 21; 1; 16; 1; 21; 1; 17; 1; 15; 0; 12; 1; 1; 1; 12; 1; 1; 0 ;
|]
let _htmlstream_char_class : int array = [|
0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 3; 4; 4; 4; 4; 5; 4; 4; 4; 4; 4; 6; 7; 8; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 7; 4; 10; 11; 12; 13; 4; 9; 9; 14; 9; 15; 9; 9; 9; 16; 9; 9; 17; 9; 9; 9; 18; 9; 19; 20; 21; 9; 9; 9; 9; 22; 9; 4; 4; 4; 4; 7; 4; 9; 9; 14; 9; 15; 9; 9; 9; 16; 9; 9; 17; 9; 9; 9; 18; 9; 19; 20; 21; 9; 9; 9; 9; 22; 9; 0 ;
|]
let _htmlstream_index_offsets : int array = [|
0; 10; 20; 43; 44; 66; 72; 78; 84; 96; 118; 141; 164; 187; 210; 223; 236; 246; 249; 252; 275; 287; 292; 297; 320; 343; 366; 389; 402; 412; 425; 438; 448; 451; 454; 477; 482; 487; 510; 522; 544; 566; 576; 586; 597; 618; 632; 651; 667; 685; 706; 719; 720; 730; 740; 751; 772; 793; 815; 832; 847; 860; 861; 871; 881; 892; 914; 930; 951; 968; 983; 996; 997; 1009; 0 ;
|]
let _htmlstream_indices : int array = [|
2; 1; 1; 1; 1; 1; 1; 1; 1; 3; 6; 5; 5; 5; 5; 5; 5; 5; 5; 7; 3; 10; 11; 9; 9; 9; 12; 12; 13; 12; 9; 9; 9; 14; 12; 12; 12; 12; 12; 12; 12; 12; 12; 16; 18; 9; 9; 9; 9; 19; 9; 9; 20; 9; 9; 9; 9; 20; 20; 20; 20; 20; 20; 20; 20; 20; 18; 9; 9; 9; 9; 22; 24; 22; 22; 22; 22; 25; 24; 22; 22; 22; 22; 27; 24; 22; 22; 22; 22; 27; 22; 22; 22; 22; 22; 0; 18; 9; 9; 9; 9; 30; 30; 9; 30; 9; 9; 9; 9; 30; 30; 30; 30; 30; 30; 30; 30; 30; 32; 33; 9; 9; 9; 9; 30; 30; 9; 30; 9; 9; 34; 35; 30; 30; 30; 30; 30; 30; 30; 30; 30; 37; 38; 9; 9; 9; 9; 39; 39; 9; 39; 9; 9; 40; 41; 39; 39; 39; 39; 39; 39; 39; 39; 39; 43; 44; 9; 9; 9; 9; 45; 45; 9; 45; 9; 46; 47; 48; 45; 45; 45; 45; 45; 45; 45; 45; 45; 50; 51; 9; 9; 9; 9; 52; 52; 9; 52; 9; 53; 54; 55; 52; 52; 52; 52; 52; 52; 52; 52; 52; 53; 58; 57; 59; 57; 60; 57; 57; 57; 57; 57; 57; 9; 63; 64; 62; 9; 62; 9; 62; 62; 62; 62; 62; 62; 65; 68; 67; 67; 67; 67; 67; 67; 67; 67; 69; 72; 71; 73; 76; 75; 77; 79; 80; 9; 9; 9; 9; 52; 52; 9; 52; 9; 9; 54; 55; 52; 52; 52; 52; 52; 52; 52; 52; 52; 18; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 40; 84; 83; 83; 83; 73; 87; 86; 86; 86; 77; 89; 90; 9; 9; 9; 9; 91; 91; 92; 91; 9; 9; 93; 9; 91; 91; 91; 91; 91; 91; 91; 91; 91; 95; 96; 9; 9; 9; 9; 97; 97; 98; 97; 9; 9; 99; 9; 97; 97; 97; 97; 97; 97; 97; 97; 97; 101; 102; 9; 9; 9; 9; 103; 103; 104; 103; 9; 105; 106; 9; 103; 103; 103; 103; 103; 103; 103; 103; 103; 108; 109; 9; 9; 9; 9; 110; 110; 111; 110; 9; 112; 113; 9; 110; 110; 110; 110; 110; 110; 110; 110; 110; 98; 115; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 116; 119; 118; 118; 118; 118; 118; 118; 118; 118; 120; 112; 123; 122; 124; 122; 125; 122; 122; 122; 122; 122; 122; 9; 128; 129; 127; 9; 127; 9; 127; 127; 127; 127; 127; 127; 130; 133; 132; 132; 132; 132; 132; 132; 132; 132; 134; 137; 136; 138; 141; 140; 142; 144; 145; 9; 9; 9; 9; 110; 110; 111; 110; 9; 9; 113; 9; 110; 110; 110; 110; 110; 110; 110; 110; 110; 148; 147; 147; 147; 138; 151; 150; 150; 150; 142; 154; 155; 153; 153; 153; 153; 156; 156; 153; 156; 153; 153; 157; 153; 156; 156; 156; 156; 156; 156; 156; 156; 156; 160; 159; 159; 159; 159; 159; 159; 159; 159; 159; 159; 0; 163; 162; 162; 162; 162; 164; 164; 162; 164; 162; 162; 165; 162; 164; 164; 164; 164; 164; 164; 164; 164; 164; 18; 9; 9; 9; 9; 9; 9; 9; 20; 9; 9; 9; 9; 20; 20; 20; 20; 20; 20; 20; 20; 20; 169; 168; 168; 168; 168; 168; 168; 168; 168; 170; 172; 171; 171; 171; 171; 171; 171; 171; 171; 173; 174; 175; 171; 171; 171; 171; 171; 171; 176; 171; 173; 176; 177; 171; 171; 171; 171; 171; 171; 171; 171; 173; 171; 171; 171; 171; 171; 171; 171; 171; 171; 178; 172; 171; 171; 171; 171; 171; 171; 171; 171; 173; 171; 171; 171; 179; 172; 171; 171; 171; 171; 171; 171; 171; 171; 173; 171; 171; 171; 171; 171; 171; 171; 171; 180; 172; 171; 171; 171; 171; 171; 171; 171; 171; 173; 171; 171; 171; 171; 171; 181; 172; 171; 171; 171; 171; 171; 171; 171; 171; 173; 171; 171; 171; 171; 171; 171; 171; 182; 172; 171; 171; 171; 171; 171; 171; 171; 171; 173; 171; 171; 171; 171; 171; 171; 171; 171; 171; 171; 183; 183; 184; 171; 171; 171; 171; 171; 171; 171; 171; 173; 171; 185; 187; 190; 189; 189; 189; 189; 189; 189; 189; 189; 191; 193; 192; 192; 192; 192; 192; 192; 192; 192; 194; 195; 196; 192; 192; 192; 192; 192; 192; 197; 192; 194; 197; 198; 192; 192; 192; 192; 192; 192; 192; 192; 194; 192; 192; 192; 192; 192; 192; 192; 192; 192; 199; 193; 192; 192; 192; 192; 192; 192; 192; 192; 194; 192; 192; 192; 192; 192; 192; 192; 192; 192; 192; 200; 193; 192; 192; 192; 192; 192; 192; 192; 192; 194; 192; 192; 192; 192; 192; 192; 192; 192; 192; 192; 192; 201; 193; 192; 192; 192; 192; 192; 192; 192; 192; 194; 192; 192; 192; 192; 192; 192; 202; 193; 192; 192; 192; 192; 192; 192; 192; 192; 194; 192; 192; 192; 192; 203; 203; 204; 192; 192; 192; 192; 192; 192; 192; 192; 194; 192; 205; 207; 210; 209; 209; 209; 209; 209; 209; 209; 209; 211; 213; 212; 212; 212; 212; 212; 212; 212; 212; 214; 215; 216; 212; 212; 212; 212; 212; 212; 217; 212; 214; 217; 218; 212; 212; 212; 212; 212; 212; 212; 212; 214; 212; 212; 212; 212; 212; 212; 212; 212; 212; 212; 219; 213; 212; 212; 212; 212; 212; 212; 212; 212; 214; 212; 212; 212; 212; 212; 220; 213; 212; 212; 212; 212; 212; 212; 212; 212; 214; 212; 212; 212; 212; 212; 212; 212; 212; 212; 212; 221; 213; 212; 212; 212; 212; 212; 212; 212; 212; 214; 212; 212; 212; 212; 212; 212; 222; 213; 212; 212; 212; 212; 212; 212; 212; 212; 214; 212; 212; 212; 212; 223; 223; 224; 212; 212; 212; 212; 212; 212; 212; 212; 214; 212; 225; 227; 229; 228; 228; 228; 228; 228; 228; 228; 228; 228; 228; 230; 232; 0 ;
|]
let _htmlstream_index_defaults : int array = [|
1; 5; 9; 15; 9; 9; 22; 22; 22; 9; 9; 9; 9; 9; 57; 62; 67; 71; 75; 9; 9; 83; 86; 9; 9; 9; 9; 9; 118; 122; 127; 132; 136; 140; 9; 147; 150; 153; 159; 162; 9; 168; 171; 171; 171; 171; 171; 171; 171; 171; 171; 186; 189; 192; 192; 192; 192; 192; 192; 192; 192; 206; 209; 212; 212; 212; 212; 212; 212; 212; 212; 226; 228; 231; 0 ;
|]
let _htmlstream_cond_targs : int array = [|
0; 1; 1; 2; 1; 1; 1; 2; 2; 3; 2; 4; 23; 37; 40; 3; 3; 4; 3; 5; 9; 5; 6; 6; 6; 7; 7; 8; 8; 9; 10; 10; 11; 11; 16; 20; 11; 11; 11; 12; 16; 20; 12; 13; 13; 12; 14; 16; 20; 13; 13; 13; 12; 14; 16; 20; 14; 15; 14; 17; 21; 15; 15; 11; 11; 16; 16; 1; 1; 2; 17; 18; 18; 19; 18; 18; 18; 19; 19; 11; 11; 20; 21; 22; 22; 22; 22; 22; 23; 24; 24; 23; 27; 31; 24; 24; 24; 25; 27; 31; 25; 26; 26; 25; 27; 29; 31; 26; 26; 26; 25; 27; 29; 31; 27; 27; 28; 28; 1; 1; 2; 29; 30; 29; 32; 35; 30; 30; 24; 24; 31; 31; 1; 1; 2; 32; 33; 33; 34; 33; 33; 33; 34; 34; 24; 24; 35; 36; 36; 36; 36; 36; 37; 38; 37; 37; 39; 0; 38; 38; 38; 39; 38; 38; 39; 0; 40; 41; 42; 42; 43; 42; 42; 43; 43; 43; 44; 44; 45; 46; 47; 48; 49; 50; 50; 51; 51; 51; 52; 53; 53; 54; 53; 53; 54; 54; 54; 55; 55; 56; 57; 58; 59; 60; 60; 61; 61; 61; 62; 63; 63; 64; 63; 63; 64; 64; 64; 65; 65; 66; 67; 68; 69; 70; 70; 71; 71; 71; 72; 72; 73; 73; 73; 0 ;
|]
let _htmlstream_cond_actions : int array = [|
0; 1; 2; 0; 3; 0; 4; 3; 5; 5; 4; 6; 7; 6; 6; 0; 4; 5; 8; 0; 1; 5; 0; 5; 4; 0; 5; 0; 5; 5; 0; 5; 9; 10; 9; 9; 5; 0; 4; 1; 0; 0; 5; 11; 12; 0; 11; 13; 13; 5; 0; 4; 14; 0; 15; 15; 5; 1; 4; 0; 0; 5; 0; 16; 17; 16; 18; 19; 20; 18; 5; 1; 2; 21; 5; 0; 4; 22; 5; 15; 23; 5; 5; 1; 2; 5; 0; 4; 5; 24; 25; 0; 24; 24; 5; 0; 4; 1; 0; 0; 5; 11; 12; 0; 13; 11; 13; 5; 0; 4; 14; 15; 0; 15; 5; 4; 0; 26; 27; 28; 26; 5; 1; 4; 0; 0; 5; 0; 16; 17; 16; 29; 30; 31; 29; 5; 1; 2; 21; 5; 0; 4; 22; 5; 15; 23; 5; 1; 2; 5; 0; 4; 5; 32; 0; 4; 1; 32; 5; 0; 4; 5; 33; 34; 0; 33; 5; 0; 1; 35; 21; 0; 4; 22; 0; 4; 0; 4; 0; 0; 0; 0; 0; 0; 4; 36; 0; 4; 0; 1; 35; 21; 0; 4; 22; 0; 4; 0; 4; 0; 0; 0; 0; 0; 4; 37; 0; 4; 0; 1; 35; 21; 0; 4; 22; 0; 4; 0; 4; 0; 0; 0; 0; 0; 4; 38; 0; 4; 0; 4; 39; 0; 4; 0 ;
|]
let _htmlstream_eof_trans : int array = [|
1; 5; 9; 16; 18; 22; 24; 27; 29; 30; 32; 37; 43; 50; 57; 62; 67; 71; 75; 79; 82; 83; 86; 89; 95; 101; 108; 115; 118; 122; 127; 132; 136; 140; 144; 147; 150; 153; 159; 162; 167; 168; 172; 175; 177; 179; 180; 181; 182; 183; 184; 187; 189; 193; 196; 198; 200; 201; 202; 203; 204; 207; 209; 213; 216; 218; 220; 221; 222; 223; 224; 227; 229; 232; 0 ;
|]
let htmlstream_start  : int  = 0
let htmlstream_first_final  : int  = 0
let htmlstream_error  : int  = -1
let htmlstream_en_in_script  : int  = 41
let htmlstream_en_in_style  : int  = 52
let htmlstream_en_in_title  : int  = 62
let htmlstream_en_garbage_tag  : int  = 72
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
	mark_end = ref (-1);
	tag = ref "";
	key = ref "";
	attrs = ref [];
	directive = ref "";
	line = 1;
	tokens = Array.make buffer_capacity `EOF;
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
let directive = scanner.directive in
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
				if ( Char.code data.[p.contents] )<= 122 && ( Char.code data.[p.contents] )>= 0  then
				begin
					begin
						_ic  := _htmlstream_char_class.(( Char.code data.[p.contents] )- 0);
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
				else if _htmlstream_cond_actions.(_trans.contents) = 22  then
				begin
					begin
						mark_end := !p
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 24  then
				begin
					begin
						tag := String.lowercase_ascii @@ sub (); attrs := [];
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 33  then
				begin
					begin
						let name = String.lowercase_ascii @@ sub () in
						if name <> "br" then begin
							emit scanner (`End (make_tag name []));
							pause ()
						end;

					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 9  then
				begin
					begin
						directive := String.lowercase_ascii @@ sub (); attrs := [];
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 3  then
				begin
					begin
						emit scanner (`String (decode (sub ())));
						pause ();

					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 11  then
				begin
					begin
						key := String.lowercase_ascii @@ sub ()
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 15  then
				begin
					begin
						attrs := (!key, if !mark < 0 then "" else sub()) :: !attrs
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 29  then
				begin
					begin
						match !tag with
						| "script" -> begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 41;

						end;
						| "style" -> begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 52;

						end;
						| "title" -> begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 62;

						end;
						| "" -> ()
						| name ->
						emit scanner (`Start (make_tag name (attributes !attrs)));
						pause ();

					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 26  then
				begin
					begin
						let start = `Start (make_tag !tag (attributes !attrs)) in
						if !tag = "a" || !tag = "br" then emit scanner start
						else emit_many scanner [start; `End (make_tag !tag [])];
						pause ();

					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 18  then
				begin
					begin

					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 5  then
				begin
					begin
						begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 72;

						end;

					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 4  then
				begin
					begin
						scanner.line <- scanner.line + 1
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 6  then
				begin
					begin
						tag := ""
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 21  then
				begin
					begin
						mark := !p
					end;
					begin
						mark_end := !p
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 32  then
				begin
					begin
						mark := !p
					end;
					begin
						let name = String.lowercase_ascii @@ sub () in
						if name <> "br" then begin
							emit scanner (`End (make_tag name []));
							pause ()
						end;

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
				else if _htmlstream_cond_actions.(_trans.contents) = 16  then
				begin
					begin
						mark_end := !p
					end;
					begin
						attrs := (!key, if !mark < 0 then "" else sub()) :: !attrs
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 25  then
				begin
					begin
						tag := String.lowercase_ascii @@ sub (); attrs := [];
					end;
					begin
						scanner.line <- scanner.line + 1
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 34  then
				begin
					begin
						let name = String.lowercase_ascii @@ sub () in
						if name <> "br" then begin
							emit scanner (`End (make_tag name []));
							pause ()
						end;

					end;
					begin
						scanner.line <- scanner.line + 1
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 10  then
				begin
					begin
						directive := String.lowercase_ascii @@ sub (); attrs := [];
					end;
					begin
						scanner.line <- scanner.line + 1
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 13  then
				begin
					begin
						key := String.lowercase_ascii @@ sub ()
					end;
					begin
						attrs := (!key, if !mark < 0 then "" else sub()) :: !attrs
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 12  then
				begin
					begin
						key := String.lowercase_ascii @@ sub ()
					end;
					begin
						scanner.line <- scanner.line + 1
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 14  then
				begin
					begin
						attrs := (!key, if !mark < 0 then "" else sub()) :: !attrs
					end;
					begin
						mark := !p
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 23  then
				begin
					begin
						attrs := (!key, if !mark < 0 then "" else sub()) :: !attrs
					end;
					begin
						scanner.line <- scanner.line + 1
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 30  then
				begin
					begin
						match !tag with
						| "script" -> begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 41;

						end;
						| "style" -> begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 52;

						end;
						| "title" -> begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 62;

						end;
						| "" -> ()
						| name ->
						emit scanner (`Start (make_tag name (attributes !attrs)));
						pause ();

					end;
					begin
						mark := !p
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 39  then
				begin
					begin
						match !tag with
						| "script" -> begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 41;

						end;
						| "style" -> begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 52;

						end;
						| "title" -> begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 62;

						end;
						| "" -> ()
						| name ->
						emit scanner (`Start (make_tag name (attributes !attrs)));
						pause ();

					end;
					begin
						begin
							cs  := 0;

						end;

					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 27  then
				begin
					begin
						let start = `Start (make_tag !tag (attributes !attrs)) in
						if !tag = "a" || !tag = "br" then emit scanner start
						else emit_many scanner [start; `End (make_tag !tag [])];
						pause ();

					end;
					begin
						mark := !p
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 19  then
				begin
					begin

					end;
					begin
						mark := !p
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 8  then
				begin
					begin
						begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 72;

						end;

					end;
					begin
						scanner.line <- scanner.line + 1
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 35  then
				begin
					begin
						scanner.line <- scanner.line + 1
					end;
					begin
						mark := !p
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 36  then
				begin
					begin
						emit_many scanner
						[`Start (make_tag "script" (attributes !attrs));
						`String (sub ());
						`End (make_tag "script" [])];
						pause ();

					end;
					begin
						begin
							cs  := 0;

						end;

					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 37  then
				begin
					begin
						emit_many scanner
						[`Start (make_tag "style" (attributes !attrs));
						`String (sub ());
						`End (make_tag "style" [])];
						pause ();

					end;
					begin
						begin
							cs  := 0;

						end;

					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 38  then
				begin
					begin
						emit_many scanner
						[`Start (make_tag "title" (attributes !attrs));
						`String (decode (sub ()));
						`End (make_tag "title" [])];
						pause ();

					end;
					begin
						begin
							cs  := 0;

						end;

					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 7  then
				begin
					begin
						tag := ""
					end;
					begin
						mark := !p
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 17  then
				begin
					begin
						mark_end := !p
					end;
					begin
						attrs := (!key, if !mark < 0 then "" else sub()) :: !attrs
					end;
					begin
						scanner.line <- scanner.line + 1
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 31  then
				begin
					begin
						match !tag with
						| "script" -> begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 41;

						end;
						| "style" -> begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 52;

						end;
						| "title" -> begin
							p  := p.contents- 1;

						end;
						begin
							cs  := 62;

						end;
						| "" -> ()
						| name ->
						emit scanner (`Start (make_tag name (attributes !attrs)));
						pause ();

					end;
					begin
						mark := !p
					end;
					begin
						scanner.line <- scanner.line + 1
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 28  then
				begin
					begin
						let start = `Start (make_tag !tag (attributes !attrs)) in
						if !tag = "a" || !tag = "br" then emit scanner start
						else emit_many scanner [start; `End (make_tag !tag [])];
						pause ();

					end;
					begin
						mark := !p
					end;
					begin
						scanner.line <- scanner.line + 1
					end;

				end
				else if _htmlstream_cond_actions.(_trans.contents) = 20  then
				begin
					begin

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
if !p >= !eof then scanner.finished <- true
else if scanner.write = 0 then scanner.finished <- true

let rec next scanner (_state : Html_tokenizer.state)
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
else begin
	scanner.read <- 0;
	scanner.write <- 0;
	run scanner;
	next scanner _state location
end
