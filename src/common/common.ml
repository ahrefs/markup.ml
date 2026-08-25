(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

type 'a cont = 'a -> unit
type 'a cps = exn cont -> 'a cont -> unit

type location = int * int

let compare_locations (line, column) (line', column') =
  match line - line' with
  | 0 -> column - column'
  | order -> order

type name = string * string

let xml_ns = "http://www.w3.org/XML/1998/namespace"
let xmlns_ns = "http://www.w3.org/2000/xmlns/"
let xlink_ns = "http://www.w3.org/1999/xlink"
let html_ns = "http://www.w3.org/1999/xhtml"
let svg_ns = "http://www.w3.org/2000/svg"
let mathml_ns = "http://www.w3.org/1998/Math/MathML"

type xml_declaration =
  {version    : string;
   encoding   : string option;
   standalone : bool option}

type doctype =
  {doctype_name      : string option;
   public_identifier : string option;
   system_identifier : string option;
   raw_text          : string option;
   force_quirks      : bool}

type signal =
  [ `Start_element of name * (name * string) list
  | `End_element
  | `Text of string list
  | `Xml of xml_declaration
  | `Doctype of doctype
  | `PI of string * string
  | `Comment of string ]

let signal_to_string = function
  | `Comment s ->
    Printf.sprintf "<!--%s-->" s

  | `Doctype d ->
    let text =
      match d.doctype_name with
      | None ->
        begin match d.raw_text with
        | None -> ""
        | Some s -> " " ^ s
        end
      | Some name ->
        match d.public_identifier, d.system_identifier with
        | None, None -> " " ^ name
        | Some p, None -> Printf.sprintf " %s PUBLIC \"%s\"" name p
        | None, Some s -> Printf.sprintf " %s SYSTEM \"%s\"" name s
        | Some p, Some s -> Printf.sprintf " %s PUBLIC \"%s\" \"%s\"" name p s
    in
    Printf.sprintf "<!DOCTYPE%s>" text

  | `Start_element (name, attributes) ->
    let name_to_string = function
      | "", local_name -> local_name
      | ns, local_name -> ns ^ ":" ^ local_name
    in
    let attributes =
      attributes
      |> List.map (fun (name, value) ->
        Printf.sprintf " %s=\"%s\"" (name_to_string name) value)
      |> String.concat ""
    in
    Printf.sprintf "<%s%s>" (name_to_string name) attributes

  | `End_element ->
    "</...>"

  | `Text ss ->
    String.concat "" ss

  | `Xml x ->
    let s = Printf.sprintf "<?xml version=\"%s\">" x.version in
    let s =
      match x.encoding with
      | None -> s
      | Some encoding -> Printf.sprintf "%s encoding=\"%s\"" s encoding
    in
    let s =
      match x.standalone with
      | None -> s
      | Some standalone ->
        Printf.sprintf
          "%s standalone=\"%s\"" s (if standalone then "yes" else "no")
    in
    s ^ "?>"

  | `PI (target, s) ->
    Printf.sprintf "<?%s %s?>" target s
