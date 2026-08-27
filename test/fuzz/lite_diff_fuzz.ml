let maximum_input_length = 1024 * 1024
let depth_limit = 60

let read_input () =
  let input =
    match Sys.argv with
    | [| _; path |] -> CCIO.File.read_exn path
    | _ -> CCIO.read_all stdin
  in
  if String.length input > maximum_input_length then None else Some input

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

type outcome = Signals of Markup_common.signal list | Raised of string

let run parse collect_signals context input =
  try Signals (collect_signals (parse context input))
  with exn -> Raised (Printexc.to_string exn)

let oracle context input =
  run
    (fun context input ->
      Oracle.parse ~depth_limit ~context (fun _ _ -> ()) input)
    (collect Markup.iter) context input

let lite context input =
  run
    (fun context input ->
      Markup_lite.parse_html
        ~report:(fun _ _ -> ())
        ~context ~depth_limit input)
    (collect Markup_lite.iter) context input

let truncate string =
  let maximum = 240 in
  if String.length string <= maximum then string
  else String.sub string 0 maximum ^ "..."

let signal = function
  | None -> "<end of stream>"
  | Some signal ->
      Markup_common.signal_to_string signal |> truncate |> Printf.sprintf "%S"

let first = function [] -> None | value :: _ -> Some value

let crash format =
  Printf.ksprintf
    (fun message ->
      Printf.eprintf "markup.lite differential mismatch: %s\n%!" message;
      Unix.kill (Unix.getpid ()) Sys.sigabrt;
      exit 2)
    format

let compare_signals expected actual =
  let rec compare index expected actual =
    match (expected, actual) with
    | [], [] -> ()
    | expected :: expected_rest, actual :: actual_rest when expected = actual ->
        compare (index + 1) expected_rest actual_rest
    | expected, actual ->
        crash "signal %d: oracle=%s lite=%s" index
          (signal (first expected))
          (signal (first actual))
  in
  compare 0 expected actual

let check input =
  let context, body = context_of_input input in
  match (oracle context body, lite context body) with
  | Signals expected, Signals actual -> compare_signals expected actual
  | Raised expected, Raised actual ->
      if expected <> actual then
        crash "exceptions differ: oracle=%S lite=%S" expected actual
  | Raised exception_, Signals _ ->
      crash "oracle raised but Lite returned signals: %S" exception_
  | Signals _, Raised exception_ ->
      crash "Lite raised but oracle returned signals: %S" exception_

let () = match read_input () with Some input -> check input | None -> ()
