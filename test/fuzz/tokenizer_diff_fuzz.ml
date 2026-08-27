open Tokenizer_diff
module Kstream = Markup__Kstream

let maximum_input_length = 1024 * 1024

let read_input () =
  let input =
    match Sys.argv with
    | [| _; path |] -> CCIO.File.read_exn path
    | _ -> CCIO.read_all stdin
  in
  if String.length input > maximum_input_length then None else Some input

let pull stream =
  let result = ref None in
  Kstream.next stream raise
    (fun () -> result := Some None)
    (fun value -> result := Some (Some value));
  match !result with
  | Some result -> result
  | None -> failwith "decoder did not resume synchronously"

let preprocess bytes =
  let report _location _error _throw resume = resume () in
  let stream, get_location =
    bytes |> Markup__Stream_io.string
    |> Markup__Encoding.utf_8 ~report
    |> Markup__Input.preprocess Markup__Common.is_valid_html_char report
  in
  let rec drain accumulator =
    match pull stream with
    | None -> (List.rev accumulator, get_location ())
    | Some scalar -> drain (scalar :: accumulator)
  in
  drain []

let check_raw_tokenizer input =
  let scalars, eof_location = preprocess input in
  let reference, candidate = compare_data ~eof_location scalars in
  if reference <> candidate then failwith "raw tokenizer mismatch";
  (candidate.tokens, scalars, eof_location)

let command byte =
  match byte mod 8 with
  | 0 -> Next
  | 1 -> Set_state `Data
  | 2 -> Set_state `RCDATA
  | 3 -> Set_state `RAWTEXT
  | 4 -> Set_state `Script_data
  | 5 -> Set_state `PLAINTEXT
  | 6 -> Set_foreign false
  | _ -> Set_foreign true

let check_script_tokenizer input =
  let length = String.length input in
  let rec decode offset index scalars commands =
    if offset + 3 >= length then (List.rev scalars, List.rev commands)
    else
      let byte position = Char.code input.[offset + position] in
      let scalar = byte 0 lor (byte 1 lsl 8) lor (byte 2 lsl 16) in
      decode (offset + 4) (index + 1)
        (((1 + (index mod 17), 1 + (index mod 71)), scalar) :: scalars)
        (command (byte 3) :: commands)
  in
  let scalars, commands = decode 1 0 [] [] in
  let commands =
    commands @ List.init (List.length scalars + 8) (fun _ -> Next)
  in
  let eof_location = (1 + (length mod 17), 1 + (length mod 71)) in
  let reference, candidate = script ~eof_location scalars commands in
  if reference <> candidate then failwith "scripted tokenizer mismatch";
  candidate.tokens

let token_tag byte =
  let names = [| "p"; "table"; "tr"; "td"; "svg"; "math"; "b"; "script" |] in
  Markup_lite.Token_tag.
    {
      name = names.(byte mod Array.length names);
      attributes =
        (if byte land 8 = 0 then []
         else [ ("a", String.make 1 (Char.chr byte)) ]);
      self_closing = byte land 16 <> 0;
    }

let token byte =
  match byte mod 8 with
  | 0 -> `Char byte
  | 1 -> `String (String.make 1 (Char.chr byte))
  | 2 -> `Start (token_tag byte)
  | 3 -> `End (token_tag byte)
  | 4 -> `Comment (String.make 1 (Char.chr byte))
  | 5 ->
      `Doctype
        {
          Markup_common.doctype_name = Some "html";
          public_identifier = None;
          system_identifier = None;
          raw_text = None;
          force_quirks = byte land 8 <> 0;
        }
  | 6 -> `String "\x00\x0C<![CDATA[x]]>"
  | _ -> `EOF

let baseline_tag (tag : Markup_lite.Token_tag.t) =
  Markup.Internals.Token_tag.
    {
      name = tag.name;
      attributes = tag.attributes;
      self_closing = tag.self_closing;
    }

let baseline_token : Markup_lite.token -> Markup.Internals.token = function
  | `Start tag -> `Start (baseline_tag tag)
  | `End tag -> `End (baseline_tag tag)
  | (`Doctype _ | `Char _ | `String _ | `Comment _ | `EOF) as token -> token

let lite_token : Tokenizer_diff.token -> Markup_lite.token = function
  | `Start tag ->
      `Start
        Markup_lite.Token_tag.
          {
            name = tag.name;
            attributes = tag.attributes;
            self_closing = tag.self_closing;
          }
  | `End tag ->
      `End
        Markup_lite.Token_tag.
          {
            name = tag.name;
            attributes = tag.attributes;
            self_closing = tag.self_closing;
          }
  | (`Doctype _ | `Char _ | `String _ | `Comment _ | `EOF) as token -> token

let compare_token_parsers ~context tokens =
  let baseline () =
    let reports = ref [] in
    let signals =
      tokens
      |> List.map (fun (location, token) -> (location, baseline_token token))
      |> Markup.Internals.parse_tokens ~depth_limit:60 ~context
           ~report:(fun location error ->
             reports := (location, error) :: !reports)
      |> Markup.signals |> Markup.to_list
    in
    (signals, List.rev !reports)
  in
  let lite () =
    let reports = ref [] in
    let signals = ref [] in
    Markup_lite.parse_tokens ~depth_limit:60 ~context
      ~report:(fun location error -> reports := (location, error) :: !reports)
      tokens
    |> Markup_lite.iter (fun signal -> signals := signal :: !signals);
    (List.rev !signals, List.rev !reports)
  in
  let outcome run =
    try `Parsed (run ()) with exn -> `Raised (Printexc.to_string exn)
  in
  let expected = outcome baseline in
  let actual = outcome lite in
  if expected <> actual then begin
    let summarize = function
      | `Raised message -> "raised " ^ message
      | `Parsed (signals, reports) ->
          let rendered =
            signals
            |> List.map Markup_common.signal_to_string
            |> String.concat " | "
          in
          Printf.sprintf "parsed %d signals/%d reports: %s"
            (List.length signals) (List.length reports) rendered
    in
    failwith
      (Printf.sprintf "parse_tokens mismatch: baseline %s; lite %s"
         (summarize expected) (summarize actual))
  end

let parser_context input =
  if String.length input > 1 && Char.code input.[1] land 1 <> 0 then
    `Fragment "table"
  else `Document

let check_synthetic_parser input =
  let tokens =
    String.to_seq input |> List.of_seq
    |> List.mapi (fun index byte ->
        ((1 + (index mod 13), 1 + (index mod 67)), token (Char.code byte)))
  in
  let tokens =
    if String.length input > 0 && Char.code input.[0] land 4 <> 0 then tokens
    else tokens @ [ ((1, String.length input + 1), `EOF) ]
  in
  compare_token_parsers ~context:(parser_context input) tokens

let check input =
  let context = parser_context input in
  if String.length input = 0 || Char.code input.[0] land 1 = 0 then begin
    let tokens, scalars, eof_location = check_raw_tokenizer input in
    compare_token_parsers ~context
      (List.map (fun (location, token) -> (location, lite_token token)) tokens);
    ignore (shadow_parse ~depth_limit:60 None ~eof_location scalars)
  end
  else begin
    let tokens = check_script_tokenizer input in
    compare_token_parsers ~context
      (List.map (fun (location, token) -> (location, lite_token token)) tokens)
  end;
  check_synthetic_parser input

let () = match read_input () with Some input -> check input | None -> ()
