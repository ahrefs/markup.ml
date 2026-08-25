(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(* The stream type shared by the libraries built on markup.common. This module
   is only the type; the operations on streams (in [Kstream]) remain private
   to each library.

   [t] is abstract here, which is what makes the phantom parameter meaningful:
   outside this library, [('a, sync) t] and [('a, async) t] cannot be shown
   equal, so a synchronous stream cannot be passed where an asynchronous one is
   expected, and vice versa. That abstraction is the only thing keeping the two
   distinct outside this library. *)

type ('a, 's) t

module Private :
sig
  (* The two identity conversions between a [Kstream.t] and a phantom-tagged
     [Stream.t], exposed so that a library implementing streams on top of
     [Kstream] can exchange streams with other such libraries at no cost.
     Not a stable interface. *)
  val to_stream : 'a Kstream.t -> ('a, 's) t
  val of_stream : ('a, 's) t -> 'a Kstream.t
end
