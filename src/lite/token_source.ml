(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

type location_out = { mutable line : int; mutable column : int }
type pushed_token = { token : Html_tokenizer.token; line : int; column : int }

type t = {
  stream :
    (Markup_common.location * Markup__Html_tokenizer.token) Markup__Kstream.t;
  set_state : Markup__Html_tokenizer.state -> unit;
  set_foreign : (unit -> bool) -> unit;
  mutable pushed : pushed_token list;
}

let baseline_tag (tag : Markup__Common.Token_tag.t) =
  {
    Common.Token_tag.name = tag.name;
    attributes = tag.attributes;
    self_closing = tag.self_closing;
  }

let token : Markup__Html_tokenizer.token -> Html_tokenizer.token = function
  | `Start tag -> `Start (baseline_tag tag)
  | `End tag -> `End (baseline_tag tag)
  | (`Doctype _ | `Char _ | `String _ | `Comment _ | `EOF) as token -> token

let create report html =
  let input, get_location =
    html |> Markup__Stream_io.string
    |> Markup__Encoding.utf_8 ~report
    |> Markup__Input.preprocess Markup__Common.is_valid_html_char report
  in
  let stream, set_state, set_foreign =
    Markup__Html_tokenizer.tokenize report (input, get_location)
  in
  { stream; set_state; set_foreign; pushed = [] }

let of_tokens tokens =
  let pushed =
    List.map (fun ((line, column), token) -> { token; line; column }) tokens
  in
  let stream = Markup__Kstream.empty () in
  { stream; set_state = ignore; set_foreign = ignore; pushed }

let location () = { line = 1; column = -1 }

let pull source =
  let result = ref None in
  Markup__Kstream.next source.stream
    (fun exn -> result := Some (`Exception exn))
    (fun () -> result := Some `End)
    (fun token -> result := Some (`Token token));
  match !result with
  | Some result -> result
  | None -> failwith "shared tokenizer did not resume synchronously"

let next source state (out : location_out) =
  match source.pushed with
  | { token; line; column } :: rest ->
      source.pushed <- rest;
      out.line <- line;
      out.column <- column;
      token
  | [] ->
      begin match state with
      | Html_tokenizer.Data -> ()
      | RCDATA -> source.set_state `RCDATA
      | RAWTEXT -> source.set_state `RAWTEXT
      | Script_data -> source.set_state `Script_data
      | PLAINTEXT -> source.set_state `PLAINTEXT
      end;
      begin match pull source with
      | `Token ((line, column), value) ->
          out.line <- line;
          out.column <- column;
          token value
      | `Exception exn -> raise exn
      | `End -> failwith "shared tokenizer ended without an EOF token"
      end

let set_foreign source foreign = source.set_foreign foreign

let push source ((line, column), token) =
  source.pushed <- { token; line; column } :: source.pushed
