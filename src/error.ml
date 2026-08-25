(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

include Markup_common.Error

open Common

type 'a handler = 'a -> t -> unit cps
type parse_handler = location handler
type write_handler = (signal * int) handler

let ignore_errors _ _ _ resume = resume ()

let report_if report condition location detail throw k =
  if condition then report location (detail ()) throw k
  else k ()
