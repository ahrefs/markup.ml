(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Performance_common
open Nethtml

let (|>) x f = f x

let parse file =
  file
  |> open_in
  |> Lexing.from_channel
  |> parse_document ~dtd:relaxed_html40_dtd
  |> ignore

let () = do_full_benchmark "nethtml" ~parse_html:parse
