(* Investigation snapshots produced by the frozen Devkit/Ragel tokenizer. *)

type t = {
  source : string;
  input : string option;
  tokens : (Markup__Common.location * Markup__Html_tokenizer.token) list;
}
