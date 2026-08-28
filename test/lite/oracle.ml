module HS = Devkit.HtmlStream

(* Raw-text tokenizer states substitute U+FFFD for U+0000. *)
let raw_text text =
  if String.contains text '\x00' then
    String.concat "\xEF\xBF\xBD" (String.split_on_char '\x00' text)
  else text

let decode raw =
  let inner = HS.Raw.project raw in
  try Devkit.Web.htmldecode inner with _ -> inner

(* See [htmlstream-adapter.md] for the common adaptation policy and its
   test-only suitability assessment. HtmlStream does not expose comments,
   self-closing syntax, raw-text token boundaries, or columns. The strict
   parser oracle therefore omits comments, defaults [self_closing] to false,
   expands Script/Style into three tokens, and uses [(line, -1)] for both
   parsers. *)
let adapt html : (Markup.location * Markup.Internals.token) list =
  let ctx = HS.init () in
  let tokens = ref [] in
  let emit token = tokens := ((HS.get_lnum ctx, -1), token) :: !tokens in
  let attributes attrs =
    List.rev_map
      (fun (name, value) -> (raw_text name, raw_text (decode value)))
      attrs
  in
  let tag name attributes : Markup.Internals.Token_tag.t =
    { name; attributes; self_closing = false }
  in
  (* Both tree builders assume tokenizers never leave U+0000 inside a
     [`String]; split NULs out as [`Char 0] to preserve that invariant. *)
  let emit_text text =
    String.split_on_char '\x00' text
    |> List.iteri (fun i part ->
           if i > 0 then emit (`Char 0);
           if part <> "" then emit (`String part))
  in
  let step = function
    | HS.Text raw -> emit_text (decode raw)
    | HS.Tag (name, attrs) -> emit (`Start (tag name (attributes attrs)))
    | HS.Close "br" -> ()
    | HS.Close name -> emit (`End (tag name []))
    | HS.Script (attrs, text) ->
        emit (`Start (tag "script" (attributes attrs)));
        emit (`String (raw_text text));
        emit (`End (tag "script" []))
    | HS.Style (attrs, text) ->
        emit (`Start (tag "style" (attributes attrs)));
        emit (`String (raw_text text));
        emit (`End (tag "style" []))
  in
  HS.parse ~ctx step html;
  emit `EOF;
  List.rev !tokens

let lite_tokens tokens : (Markup_lite.location * Markup_lite.token) list =
  let tag (tag : Markup.Internals.Token_tag.t) : Markup_lite.Token_tag.t =
    {
      name = tag.name;
      attributes = tag.attributes;
      self_closing = tag.self_closing;
    }
  in
  List.map
    (fun (location, token) ->
      let token : Markup_lite.token =
        match token with
        | `Doctype d -> `Doctype d
        | `Start t -> `Start (tag t)
        | `End t -> `End (tag t)
        | `Char c -> `Char c
        | `String s -> `String s
        | `Comment s -> `Comment s
        | `EOF -> `EOF
      in
      (location, token))
    tokens

let parse_adapted ?depth_limit
    ?(context : [ `Document | `Fragment of string ] = `Document) report tokens =
  tokens
  |> Markup.Internals.parse_tokens ?depth_limit ~report ~context
  |> Markup.signals

let parse_lite_adapted ?depth_limit
    ?(context : [ `Document | `Fragment of string ] = `Document) report tokens =
  tokens |> lite_tokens
  |> Markup_lite.parse_tokens ?depth_limit ~report ~context

let parse ?depth_limit
    ?(context : [ `Document | `Fragment of string ] = `Document) report html =
  Markup.string html
  |> Markup.parse_html ~report ~context ?depth_limit
  |> Markup.signals
