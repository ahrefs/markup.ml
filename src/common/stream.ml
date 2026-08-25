(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(* The phantom parameter is deliberately absent from the representation. *)
type ('a, 's) t = 'a Kstream.t

module Private =
struct
  let to_stream s = s
  let of_stream s = s
end
