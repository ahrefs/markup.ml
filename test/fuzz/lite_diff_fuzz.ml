let maximum_input_length = 1024 * 1024
let depth_limit = 60

let read_input channel =
  let buffer = Buffer.create 4096 in
  let bytes = Bytes.create 65536 in
  let rec read total =
    let count = input channel bytes 0 (Bytes.length bytes) in
    if count = 0 then Some (Buffer.contents buffer)
    else
      let total = total + count in
      if total > maximum_input_length then None
      else begin
        Buffer.add_subbytes buffer bytes 0 count;
        read total
      end
  in
  read 0

(* "FRAGMENT <name>\n<body>" parses <body> in fragment context <name>;
   anything else parses the whole input as a document. *)
let context_of_input input : [ `Document | `Fragment of string ] * string =
  let prefix = "FRAGMENT " in
  let prefix_length = String.length prefix in
  if
    String.length input >= prefix_length
    && String.sub input 0 prefix_length = prefix
  then
    match String.index_from_opt input prefix_length '\n' with
    | Some newline ->
        ( `Fragment (String.sub input prefix_length (newline - prefix_length)),
          String.sub input (newline + 1) (String.length input - newline - 1) )
    | None ->
        ( `Fragment
            (String.sub input prefix_length
               (String.length input - prefix_length)),
          "" )
  else (`Document, input)

let collect iter stream =
  let values = ref [] in
  iter (fun value -> values := value :: !values) stream;
  List.rev !values

type errors = (Markup_common.location * Markup_common.Error.t) list

type outcome =
  | Signals of Markup_common.signal list * errors
  | Raised of string * errors

let run parse collect_signals =
  let errors = ref [] in
  let report location error = errors := (location, error) :: !errors in
  try Signals (collect_signals (parse report), List.rev !errors)
  with exn -> Raised (Printexc.to_string exn, List.rev !errors)

let oracle context tokens =
  run
    (fun report -> Oracle.parse_adapted ~depth_limit ~context report tokens)
    (collect Markup.iter)

let lite context tokens =
  run
    (fun report ->
      Oracle.parse_lite_adapted ~depth_limit ~context report tokens)
    (collect Markup_lite.iter)

let truncate string =
  let maximum = 240 in
  if String.length string <= maximum then string
  else String.sub string 0 maximum ^ "..."

let signal = function
  | None -> "<end of stream>"
  | Some signal ->
      Markup_common.signal_to_string signal |> truncate |> Printf.sprintf "%S"

let error = function
  | None -> "<end of errors>"
  | Some ((line, column), error) ->
      Printf.sprintf "(%d,%d) %s" line column
        (truncate (Markup_common.Error.to_string error))

let first = function [] -> None | value :: _ -> Some value

let crash format =
  Printf.ksprintf
    (fun message ->
      Printf.eprintf "markup.lite differential mismatch: %s\n%!" message;
      Unix.kill (Unix.getpid ()) Sys.sigabrt;
      exit 2)
    format

let compare_lists what to_string expected actual =
  let rec compare index expected actual =
    match (expected, actual) with
    | [], [] -> ()
    | expected :: expected_rest, actual :: actual_rest when expected = actual ->
        compare (index + 1) expected_rest actual_rest
    | expected, actual ->
        crash "%s %d: oracle=%s lite=%s" what index
          (to_string (first expected))
          (to_string (first actual))
  in
  compare 0 expected actual

let compare_signals = compare_lists "signal" signal
let compare_errors = compare_lists "error" error

let compare_with_oracle expected expected_errors = function
  | Signals (actual, actual_errors) ->
      compare_signals expected actual;
      compare_errors expected_errors actual_errors
  | Raised (exception_, _) -> crash "Lite raised: %S" exception_

let check input =
  let context, body = context_of_input input in
  (* HtmlStream adaptation is intentionally performed once. *)
  let tokens = Oracle.adapt body in
  match oracle context tokens with
  | Raised _ -> ()
  | Signals (expected, expected_errors) ->
      compare_with_oracle expected expected_errors (lite context tokens)

let () = match read_input stdin with Some input -> check input | None -> ()
