module HS = Devkit.HtmlStream

let decode raw =
  let inner = HS.Raw.project raw in
  try Devkit.Web.htmldecode inner with _ -> inner

(* HtmlStream does not expose comments, self-closing syntax, raw-text token
   boundaries, or columns. The strict parser oracle therefore omits comments,
   defaults [self_closing] to false, expands Script/Style into three tokens,
   and uses [(line, -1)] for both parsers. *)
let adapt html : (Markup.location * Markup.Internals.token) list =
  let ctx = HS.init () in
  let tokens = ref [] in
  let emit token = tokens := ((HS.get_lnum ctx, -1), token) :: !tokens in
  let attributes attrs =
    List.rev_map (fun (name, value) -> (name, decode value)) attrs
  in
  let tag name attributes : Markup.Internals.Token_tag.t =
    { name; attributes; self_closing = false }
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
