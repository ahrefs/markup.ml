(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

type location_out = Markup__Html_tokenizer.location_out = {
  mutable line : int;
  mutable column : int;
}

type pushed_token = { token : Html_tokenizer.token; line : int; column : int }

exception End_of_input

type t = {
  tokenizer : Markup__Html_tokenizer.pull option;
  mutable pushed : pushed_token list;
  end_on_empty : bool;
}

let create report html =
  let input, get_location =
    html |> Markup__Stream_io.string
    |> Markup__Encoding.utf_8 ~report
    |> Markup__Input.preprocess Markup__Common.is_valid_html_char report
  in
  let tokenizer =
    Markup__Html_tokenizer.create_pull report (input, get_location)
  in
  { tokenizer = Some tokenizer; pushed = []; end_on_empty = false }

let of_tokens tokens =
  let pushed =
    List.map (fun ((line, column), token) -> { token; line; column }) tokens
  in
  { tokenizer = None; pushed; end_on_empty = true }

let location () = { line = 1; column = -1 }

let next source state (out : location_out) =
  match source.pushed with
  | { token; line; column } :: rest ->
      source.pushed <- rest;
      out.line <- line;
      out.column <- column;
      token
  | [] when source.end_on_empty -> raise End_of_input
  | [] ->
      let tokenizer = Option.get source.tokenizer in
      begin match state with
      | Html_tokenizer.Data -> ()
      | RCDATA -> Markup__Html_tokenizer.set_state tokenizer `RCDATA
      | RAWTEXT -> Markup__Html_tokenizer.set_state tokenizer `RAWTEXT
      | Script_data -> Markup__Html_tokenizer.set_state tokenizer `Script_data
      | PLAINTEXT -> Markup__Html_tokenizer.set_state tokenizer `PLAINTEXT
      end;
      Markup__Html_tokenizer.next tokenizer out

let set_foreign source foreign =
  match source.tokenizer with
  | Some tokenizer -> Markup__Html_tokenizer.set_foreign tokenizer foreign
  | None -> ()

let push source ((line, column), token) =
  source.pushed <- { token; line; column } :: source.pushed
