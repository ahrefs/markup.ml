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

module Token_tag = Common.Token_tag

type token = Html_tokenizer.token

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

let parse_html ?(report = fun _ _ -> ())
    ?(context : [ `Document | `Fragment of string ] = `Document) ?depth_limit
    html =
  Token_source.create (wrap_report report) html
  |> parse_source report context depth_limit

let parse_tokens ?(report = fun _ _ -> ())
    ?(context : [ `Document | `Fragment of string ] = `Document) ?depth_limit
    tokens =
  Token_source.of_tokens tokens |> parse_source report context depth_limit

let iter f stream =
  stream |> Markup_common.Stream.Private.of_stream
  |> Kstream.iter (fun value _ continue ->
      f value;
      continue ())
  |> fun iterate -> iterate raise ignore

let write_html ?escape_attribute ?escape_text buffer signals =
  Html_writer.write ?escape_attribute ?escape_text buffer signals
