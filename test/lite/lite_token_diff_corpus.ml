module Kstream = Markup__Kstream

type outcome =
  | Parsed of
      Markup_common.signal list * (Markup_common.location * Markup.Error.t) list
  | Raised of string

let pull stream =
  let result = ref None in
  Kstream.next stream
    (fun exn -> result := Some (`Exception exn))
    (fun () -> result := Some `End)
    (fun value -> result := Some (`Value value));
  match !result with
  | Some result -> result
  | None -> failwith "stream did not resume synchronously"

let preprocess bytes =
  let report _location _error _throw resume = resume () in
  let stream, get_location =
    bytes |> Markup__Stream_io.string
    |> Markup__Encoding.utf_8 ~report
    |> Markup__Input.preprocess Markup__Common.is_valid_html_char report
  in
  (stream, get_location)

let tokenize bytes =
  let input = preprocess bytes in
  let report _location _error _throw resume = resume () in
  let tokens, set_state, set_foreign =
    Markup__Html_tokenizer.tokenize report input
  in
  let recorded = ref [] in
  let recording_stream =
    Kstream.make (fun throw ended emit ->
        Kstream.next tokens throw ended (fun token ->
            recorded := token :: !recorded;
            emit token))
  in
  let signals =
    Markup__Html_parser.parse None report
      (recording_stream, set_state, set_foreign)
  in
  let rec drain () =
    match pull signals with
    | `Value _ -> drain ()
    | `End -> List.rev !recorded
    | `Exception exn -> raise exn
  in
  drain ()

let baseline_tag (tag : Markup__Common.Token_tag.t) =
  Markup.Internals.Token_tag.
    {
      name = tag.name;
      attributes = tag.attributes;
      self_closing = tag.self_closing;
    }

let baseline_token : Markup__Html_tokenizer.token -> Markup.Internals.token =
  function
  | `Start tag -> `Start (baseline_tag tag)
  | `End tag -> `End (baseline_tag tag)
  | (`Doctype _ | `Char _ | `String _ | `Comment _ | `EOF) as token -> token

let lite_tag (tag : Markup__Common.Token_tag.t) =
  Markup_lite.Token_tag.
    {
      name = tag.name;
      attributes = tag.attributes;
      self_closing = tag.self_closing;
    }

let lite_token : Markup__Html_tokenizer.token -> Markup_lite.token = function
  | `Start tag -> `Start (lite_tag tag)
  | `End tag -> `End (lite_tag tag)
  | (`Doctype _ | `Char _ | `String _ | `Comment _ | `EOF) as token -> token

let run_baseline tokens =
  let reports = ref [] in
  try
    let signals =
      tokens
      |> List.map (fun (location, token) -> (location, baseline_token token))
      |> Markup.Internals.parse_tokens
           ~report:(fun location error ->
             reports := (location, error) :: !reports)
           ~context:`Document
      |> Markup.signals |> Markup.to_list
    in
    Parsed (signals, List.rev !reports)
  with exn -> Raised (Printexc.to_string exn)

let run_lite tokens =
  let reports = ref [] in
  try
    let signals = ref [] in
    tokens
    |> List.map (fun (location, token) -> (location, lite_token token))
    |> Markup_lite.parse_tokens
         ~report:(fun location error ->
           reports := (location, error) :: !reports)
         ~context:`Document
    |> Markup_lite.iter (fun signal -> signals := signal :: !signals);
    Parsed (List.rev !signals, List.rev !reports)
  with exn -> Raised (Printexc.to_string exn)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let rec html_files path =
  if Sys.is_directory path then
    Sys.readdir path |> Array.to_list |> List.sort String.compare
    |> List.concat_map (fun name -> html_files (Filename.concat path name))
  else if Filename.check_suffix path ".html" then [ path ]
  else []

let () =
  let root = if Array.length Sys.argv > 1 then Sys.argv.(1) else "big_tests" in
  let files = html_files root in
  if List.length files <> 840 then
    failwith
      (Printf.sprintf "expected 840 HTML files, found %d" (List.length files));
  List.iteri
    (fun index path ->
      let tokens = read_file path |> tokenize in
      let baseline = run_baseline tokens in
      let lite = run_lite tokens in
      if baseline <> lite then begin
        Printf.eprintf "parse_tokens mismatch in %s\n%!" path;
        exit 1
      end;
      if (index + 1) mod 50 = 0 then
        Printf.eprintf "checked %d/840\r%!" (index + 1))
    files;
  Printf.printf "Baseline and Lite parse_tokens matched on 840 token streams.\n"
