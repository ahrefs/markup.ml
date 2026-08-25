(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(* Interface module of the [markup.entities] library. It only re-exports the
   library's modules so that they are reachable as [Markup_entities.Entities]
   and [Markup_entities.Trie]. *)

module Entities = Entities
module Trie = Trie
