let maximum_input_length = 1024 * 1024

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

let valid_utf_8 input =
  try
    Uutf.String.fold_utf_8
      (fun () _ -> function `Uchar _ -> () | `Malformed _ -> raise Exit)
      () input;
    true
  with Exit -> false

let collect iter stream =
  let values = ref [] in
  iter (fun value -> values := value :: !values) stream;
  List.rev !values

type outcome = Signals of Markup_common.signal list | Raised of string

let run parse collect_signals input =
  try Signals (collect_signals (parse input))
  with exn -> Raised (Printexc.to_string exn)

let oracle input =
  run
    (fun input -> Oracle.parse (fun _ _ -> ()) input)
    (collect Markup.iter) input

let lite input =
  run
    (fun input -> Markup_lite.parse_html input)
    (collect Markup_lite.iter) input

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
  match (oracle input, lite input) with
  | Signals expected, Signals actual -> compare_signals expected actual
  | Raised expected, Raised actual when expected = actual -> ()
  | Raised expected, Raised actual ->
      crash "exceptions differ: oracle=%S lite=%S" expected actual
  | Raised exception_, Signals _ ->
      crash "oracle raised but Lite returned signals: %S" exception_
  | Signals _, Raised exception_ ->
      crash "Lite raised but oracle returned signals: %S" exception_

let () =
  match read_input stdin with
  | Some input when valid_utf_8 input -> check input
  | Some _ | None -> ()
