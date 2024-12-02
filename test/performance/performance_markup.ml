(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Performance_common
open Markup

let (|>) x f = f x

let () =
  do_full_benchmark "markup.ml"
    ~parse_html:(fun page ->
      file page |> fst |> parse_html |> signals |> drain)
    ~parse_xml:(fun page ->
      file page |> fst |> parse_xml |> signals |> drain)
