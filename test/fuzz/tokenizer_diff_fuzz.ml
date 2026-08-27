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

let check_raw input =
  let scalars, eof_location = preprocess input in
  ignore (shadow_parse ~depth_limit:60 None ~eof_location scalars)

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

let check_script input =
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
  if reference <> candidate then failwith "scripted tokenizer mismatch"

let check input =
  if String.length input = 0 || Char.code input.[0] land 1 = 0 then
    check_raw input
  else check_script input

let () = match read_input () with Some input -> check input | None -> ()
