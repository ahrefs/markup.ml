(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

type async = Markup_common.async
type sync = Markup_common.sync
type ('data, 'sync) stream = ('data, 'sync) Markup_common.stream
type location = Markup_common.location
type name = Markup_common.name

type xml_declaration = Markup_common.xml_declaration = {
  version : string;
  encoding : string option;
  standalone : bool option;
}

type doctype = Markup_common.doctype = {
  doctype_name : string option;
  public_identifier : string option;
  system_identifier : string option;
  raw_text : string option;
  force_quirks : bool;
}

type signal = Markup_common.signal
type encoding = [ `Auto | `UTF_8 | `UTF_16BE | `UTF_16LE ]

module Token_tag = Common.Token_tag

type token =
  [ `Doctype of doctype
  | `Start of Token_tag.t
  | `End of Token_tag.t
  | `Char of int
  | `String of string
  | `Comment of string
  | `EOF ]

module Error = Markup_common.Error
module Ns = Markup_common.Ns

let signal_to_string = Markup_common.signal_to_string

let wrap_report report location error throw resume =
  match report location error with
  | () -> resume ()
  | exception exn -> throw exn

let parse_source report context depth_limit tokens =
  Html_parser.parse ?depth_limit context (wrap_report report) tokens
  |> Kstream.map (fun (_, signal) _ continue -> continue signal)
  |> Markup_common.Stream.Private.to_stream
  |> fun stream -> (stream : (signal, sync) stream)

let parse_html ?(report = fun _ _ -> ()) ?(encoding = `Auto)
    ?(context : [ `Document | `Fragment of string ] = `Document) ?depth_limit
    html =
  let source =
    match encoding with
    | `Auto -> Token_source.create html
    | `UTF_8 -> Token_source.create_utf_8 html
    | `UTF_16BE -> Encoding.utf_16be html |> Token_source.create_utf_8
    | `UTF_16LE -> Encoding.utf_16le html |> Token_source.create_utf_8
  in
  parse_source report context depth_limit source

let parse_tokens ?(report = fun _ _ -> ())
    ?(context : [ `Document | `Fragment of string ] = `Document) ?depth_limit
    tokens =
  let adapt (location, token) =
    let token =
      match token with
      | `Doctype d -> Html_tokenizer.Doctype d
      | `Start t -> Html_tokenizer.Start t
      | `End t -> Html_tokenizer.End t
      | `Char c -> Html_tokenizer.Char c
      | `String s -> Html_tokenizer.String s
      | `Comment s -> Html_tokenizer.Comment s
      | `EOF -> Html_tokenizer.EOF
    in
    (location, token)
  in
  tokens |> List.map adapt |> Token_source.of_tokens
  |> parse_source report context depth_limit

let iter f stream =
  stream |> Markup_common.Stream.Private.of_stream
  |> Kstream.iter (fun value _ continue ->
      f value;
      continue ())
  |> fun iterate -> iterate raise ignore

let write_html ?escape_attribute ?escape_text buffer signals =
  Html_writer.write ?escape_attribute ?escape_text buffer signals
