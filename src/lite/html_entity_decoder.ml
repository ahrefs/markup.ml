(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

let replacement = Uchar.of_int 0xFFFD

let html4_names =
  "lt gt amp quot apos nbsp iexcl cent pound curren yen brvbar sect uml copy \
   ordf laquo not shy reg macr deg plusmn sup2 sup3 acute micro para middot \
   cedil sup1 ordm raquo frac14 frac12 frac34 iquest Agrave Aacute Acirc \
   Atilde Auml Aring AElig Ccedil Egrave Eacute Ecirc Euml Igrave Iacute Icirc \
   Iuml ETH Ntilde Ograve Oacute Ocirc Otilde Ouml times Oslash Ugrave Uacute \
   Ucirc Uuml Yacute THORN szlig agrave aacute acirc atilde auml aring aelig \
   ccedil egrave eacute ecirc euml igrave iacute icirc iuml eth ntilde ograve \
   oacute ocirc otilde ouml divide oslash ugrave uacute ucirc uuml yacute \
   thorn yuml fnof Alpha Beta Gamma Delta Epsilon Zeta Eta Theta Iota Kappa \
   Lambda Mu Nu Xi Omicron Pi Rho Sigma Tau Upsilon Phi Chi Psi Omega alpha \
   beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu xi omicron \
   pi rho sigmaf sigma tau upsilon phi chi psi omega thetasym upsih piv bull \
   hellip prime Prime oline frasl weierp image real trade alefsym larr uarr \
   rarr darr harr crarr lArr uArr rArr dArr hArr forall part exist empty nabla \
   isin notin ni prod sum minus lowast radic prop infin ang and or cap cup int \
   there4 sim cong asymp ne equiv le ge sub sup nsub sube supe oplus otimes \
   perp sdot lceil rceil lfloor rfloor lang rang loz spades clubs hearts diams \
   OElig oelig Scaron scaron Yuml circ tilde ensp emsp thinsp zwnj zwj lrm rlm \
   ndash mdash lsquo rsquo sbquo ldquo rdquo bdquo dagger Dagger permil lsaquo \
   rsaquo euro"

let split_words s =
  let rec scan start index words =
    if index = String.length s then
      if start = index then List.rev words
      else List.rev (String.sub s start (index - start) :: words)
    else if s.[index] = ' ' then
      if start = index then scan (index + 1) (index + 1) words
      else
        scan (index + 1) (index + 1)
          (String.sub s start (index - start) :: words)
    else scan start (index + 1) words
  in
  scan 0 0 []

let named_entities =
  lazy
    (let names = Hashtbl.create 253 in
     List.iter (fun name -> Hashtbl.add names name ()) (split_words html4_names);
     let entities = Hashtbl.create 253 in
     Array.iter
       (fun (name, value) ->
         if Hashtbl.mem names name then Hashtbl.replace entities name value)
       Markup_entities.Entities.entities;
     (* HTML5 changed [lang] and [rang], and its legacy no-semicolon [sup1]
        entry collides with [sup] in the generated table. *)
     Hashtbl.replace entities "sup" (`One 0x2283);
     Hashtbl.replace entities "lang" (`One 0x2329);
     Hashtbl.replace entities "rang" (`One 0x232A);
     entities)

let add_uchar buffer codepoint =
  let uchar =
    try Uchar.of_int codepoint with Invalid_argument _ -> replacement
  in
  Uutf.Buffer.add_utf_8 buffer uchar

let add_utf_8 buffer text position length =
  Uutf.String.fold_utf_8 ~pos:position ~len:length
    (fun () _ -> function
      | `Uchar uchar -> Uutf.Buffer.add_utf_8 buffer uchar
      | `Malformed _ -> invalid_arg "malformed UTF-8")
    () text

let is_letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
let is_decimal = function '0' .. '9' -> true | _ -> false

let is_hexadecimal = function
  | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
  | _ -> false

let hexadecimal_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | _ -> assert false

let decode text =
  let length = String.length text in
  let buffer = Buffer.create length in
  let rec search copied index =
    if index >= length then add_utf_8 buffer text copied (length - copied)
    else if text.[index] <> '&' then search copied (index + 1)
    else
      match reference_end text (index + 1) with
      | None -> search copied (index + 1)
      | Some (after, value) ->
          add_utf_8 buffer text copied (index - copied);
          begin match value with
          | `Codepoint codepoint -> add_uchar buffer codepoint
          | `Name name -> begin
              match Hashtbl.find_opt (Lazy.force named_entities) name with
              | Some (`One codepoint) -> add_uchar buffer codepoint
              | Some (`Two (first, second)) ->
                  add_uchar buffer first;
                  add_uchar buffer second
              | None -> Uutf.Buffer.add_utf_8 buffer replacement
            end
          end;
          search after after
  and reference_end text start =
    if start >= length then None
    else if text.[start] = '#' then numeric_reference text (start + 1)
    else
      let finish = consume_while text start is_letter in
      if finish > start && finish < length && text.[finish] = ';' then
        Some (finish + 1, `Name (String.sub text start (finish - start)))
      else None
  and numeric_reference text start =
    if start >= length then None
    else if text.[start] = 'x' || text.[start] = 'X' then
      let digits = start + 1 in
      let finish = consume_while text digits is_hexadecimal in
      if finish > digits && finish < length && text.[finish] = ';' then begin
        let value = ref 0 in
        for index = digits to finish - 1 do
          value := (!value lsl 4) lor hexadecimal_value text.[index]
        done;
        Some (finish + 1, `Codepoint !value)
      end
      else None
    else
      let finish = consume_while text start is_decimal in
      if finish > start && finish < length && text.[finish] = ';' then
        Some
          ( finish + 1,
            `Codepoint (int_of_string (String.sub text start (finish - start)))
          )
      else None
  and consume_while text index predicate =
    if index < length && predicate text.[index] then
      consume_while text (index + 1) predicate
    else index
  in
  search 0 0;
  Buffer.contents buffer
