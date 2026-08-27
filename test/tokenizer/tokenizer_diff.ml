module Candidate = Markup__Html_tokenizer
module Candidate_common = Markup__Common
module Candidate_kstream = Markup__Kstream
module Parser = Markup__Html_parser
module Reference = Markup_ref_tokenizer.Html_tokenizer
module Reference_common = Markup_ref_tokenizer.Common
module Reference_kstream = Markup_ref_tokenizer.Kstream

type state = Candidate.state
type command = Next | Set_state of state | Set_foreign of bool

type tag = {
  name : string;
  attributes : (string * string) list;
  self_closing : bool;
}

type token =
  [ `Doctype of Markup_common.doctype
  | `Start of tag
  | `End of tag
  | `Char of int
  | `String of string
  | `Comment of string
  | `EOF ]

type termination = [ `EOF | `Exception of string | `Timeout ]

type outcome = {
  tokens : (Markup_common.location * token) list;
  reports : (Markup_common.location * Markup_common.Error.t) list;
  termination : termination;
}

let candidate_token : Candidate.token -> token = function
  | `Start tag ->
      `Start
        {
          name = tag.Candidate_common.Token_tag.name;
          attributes = tag.attributes;
          self_closing = tag.self_closing;
        }
  | `End tag ->
      `End
        {
          name = tag.Candidate_common.Token_tag.name;
          attributes = tag.attributes;
          self_closing = tag.self_closing;
        }
  | (`Doctype _ | `Char _ | `String _ | `Comment _ | `EOF) as token -> token

let reference_token : Reference.token -> token = function
  | `Start tag ->
      `Start
        {
          name = tag.Reference_common.Token_tag.name;
          attributes = tag.attributes;
          self_closing = tag.self_closing;
        }
  | `End tag ->
      `End
        {
          name = tag.Reference_common.Token_tag.name;
          attributes = tag.attributes;
          self_closing = tag.self_closing;
        }
  | (`Doctype _ | `Char _ | `String _ | `Comment _ | `EOF) as token -> token

let exception_id exn = Printexc.to_string exn

let pull next stream =
  let result = ref None in
  next stream
    (fun exn -> result := Some (`Exception exn))
    (fun () -> result := Some `End)
    (fun token -> result := Some (`Token token));
  match !result with
  | Some result -> result
  | None -> failwith "tokenizer did not resume synchronously"

let reporter reports location error _throw resume =
  reports := (location, error) :: !reports;
  resume ()

let script ~eof_location scalars commands =
  let reference_reports = ref [] in
  let candidate_reports = ref [] in
  let reference_stream, reference_state, reference_foreign =
    Reference.tokenize
      (reporter reference_reports)
      (Reference_kstream.of_list scalars, fun () -> eof_location)
  in
  let candidate_stream, candidate_state, candidate_foreign =
    Candidate.tokenize
      (reporter candidate_reports)
      (Candidate_kstream.of_list scalars, fun () -> eof_location)
  in
  let foreign = ref false in
  let foreign_callback () = !foreign in
  reference_foreign foreign_callback;
  candidate_foreign foreign_callback;
  let reference_tokens = ref [] in
  let candidate_tokens = ref [] in
  let reference_termination = ref None in
  let candidate_termination = ref None in
  let record_reference = function
    | `Token (location, token) ->
        let token = reference_token token in
        reference_tokens := (location, token) :: !reference_tokens;
        if token = `EOF then reference_termination := Some `EOF
    | `End -> reference_termination := Some `EOF
    | `Exception exn ->
        reference_termination := Some (`Exception (exception_id exn))
  in
  let record_candidate = function
    | `Token (location, token) ->
        let token = candidate_token token in
        candidate_tokens := (location, token) :: !candidate_tokens;
        if token = `EOF then candidate_termination := Some `EOF
    | `End -> candidate_termination := Some `EOF
    | `Exception exn ->
        candidate_termination := Some (`Exception (exception_id exn))
  in
  List.iter
    (function
      | Set_state state ->
          reference_state state;
          candidate_state state
      | Set_foreign value -> foreign := value
      | Next ->
          if !reference_termination = None then
            record_reference (pull Reference_kstream.next reference_stream);
          if !candidate_termination = None then
            record_candidate (pull Candidate_kstream.next candidate_stream))
    commands;
  let finish tokens reports termination =
    {
      tokens = List.rev !tokens;
      reports = List.rev !reports;
      termination =
        (match !termination with Some result -> result | None -> `Timeout);
    }
  in
  ( finish reference_tokens reference_reports reference_termination,
    finish candidate_tokens candidate_reports candidate_termination )

let data_commands scalars =
  let rec loop accumulator = function
    | [] -> List.rev (Next :: accumulator)
    | _ :: rest -> loop (Next :: accumulator) rest
  in
  loop [] scalars

let compare_data ~eof_location scalars =
  script ~eof_location scalars (data_commands scalars)

let shadow_parse ?depth_limit context ~eof_location scalars =
  let reference_reports = ref [] in
  let candidate_reports = ref [] in
  let parser_reports = ref [] in
  let reference_stream, reference_state, reference_foreign =
    Reference.tokenize
      (reporter reference_reports)
      (Reference_kstream.of_list scalars, fun () -> eof_location)
  in
  let candidate_stream, candidate_state, candidate_foreign =
    Candidate.tokenize
      (reporter candidate_reports)
      (Candidate_kstream.of_list scalars, fun () -> eof_location)
  in
  let shadow_stream =
    Candidate_kstream.make (fun throw ended emit ->
        match pull Reference_kstream.next reference_stream with
        | `Exception exn -> throw exn
        | `End -> ended ()
        | `Token (reference_location, reference_token') -> (
            match pull Candidate_kstream.next candidate_stream with
            | `Exception exn -> throw exn
            | `End -> failwith "candidate tokenizer ended before reference"
            | `Token (candidate_location, candidate_token') ->
                let expected =
                  (reference_location, reference_token reference_token')
                in
                let actual =
                  (candidate_location, candidate_token candidate_token')
                in
                if expected <> actual then
                  failwith "tokenizer mismatch in parser-driven shadow mode";
                emit (candidate_location, candidate_token')))
  in
  let set_state state =
    reference_state state;
    candidate_state state
  in
  let set_foreign callback =
    reference_foreign callback;
    candidate_foreign callback
  in
  let parser_report location error _throw resume =
    parser_reports := (location, error) :: !parser_reports;
    resume ()
  in
  let signals =
    Parser.parse ?depth_limit context parser_report
      (shadow_stream, set_state, set_foreign)
  in
  let termination = ref (`Timeout : termination) in
  let rec drain () =
    match pull Candidate_kstream.next signals with
    | `Token _ -> drain ()
    | `End -> termination := `EOF
    | `Exception exn -> termination := `Exception (exception_id exn)
  in
  drain ();
  if List.rev !reference_reports <> List.rev !candidate_reports then
    failwith "tokenizer report mismatch in parser-driven shadow mode";
  (!termination, List.rev !parser_reports)
