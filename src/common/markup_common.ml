(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

module Kstream = Kstream
module Error = Error
module Stream = Stream

type async = unit
type sync = unit
type ('a, 's) stream = ('a, 's) Stream.t

type location = Common.location

let compare_locations = Common.compare_locations

type name = Common.name

type xml_declaration = Common.xml_declaration = {
  version : string;
  encoding : string option;
  standalone : bool option;
}

type doctype = Common.doctype = {
  doctype_name : string option;
  public_identifier : string option;
  system_identifier : string option;
  raw_text : string option;
  force_quirks : bool;
}

type signal = Common.signal

let signal_to_string = Common.signal_to_string

module Ns = struct
  let html = Common.html_ns
  let svg = Common.svg_ns
  let mathml = Common.mathml_ns
  let xml = Common.xml_ns
  let xmlns = Common.xmlns_ns
  let xlink = Common.xlink_ns
end
