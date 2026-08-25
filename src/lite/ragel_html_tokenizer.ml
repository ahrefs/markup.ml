(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

module HS = Devkit.HtmlStream

let decode raw =
  let inner = HS.Raw.project raw in
  try Devkit.Web.htmldecode inner with _ -> inner

let tokenize html : (location * Html_tokenizer.token) list =
  let ctx = HS.init () in
  let tokens = ref [] in
  let emit token = tokens := ((HS.get_lnum ctx, -1), token) :: !tokens in
  let attributes attrs =
    List.rev_map (fun (name, value) -> name, decode value) attrs
  in
  let tag name attributes =
    {Token_tag.name; attributes; self_closing = false}
  in
  let step = function
    | HS.Text raw -> emit (`String (decode raw))
    | HS.Tag (name, attrs) -> emit (`Start (tag name (attributes attrs)))
    | HS.Close "br" -> ()
    | HS.Close name -> emit (`End (tag name []))
    | HS.Script (attrs, text) ->
      emit (`Start (tag "script" (attributes attrs)));
      emit (`String text);
      emit (`End (tag "script" []))
    | HS.Style (attrs, text) ->
      emit (`Start (tag "style" (attributes attrs)));
      emit (`String text);
      emit (`End (tag "style" []))
  in
  HS.parse ~ctx step html;
  emit `EOF;
  List.rev !tokens
