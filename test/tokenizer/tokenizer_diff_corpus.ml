open Tokenizer_diff
module Kstream = Markup__Kstream

let pull stream =
  let result = ref None in
  Kstream.next stream
    (fun exn -> raise exn)
    (fun () -> result := Some None)
    (fun value -> result := Some (Some value));
  match !result with
  | Some result -> result
  | None -> failwith "input stream did not resume synchronously"

let preprocess bytes =
  let reports = ref [] in
  let report location error _throw resume =
    reports := (location, error) :: !reports;
    resume ()
  in
  let stream, get_location =
    bytes |> Markup__Stream_io.string
    |> Markup__Encoding.utf_8 ~report
    |> Markup__Input.preprocess Markup__Common.is_valid_html_char report
  in
  let rec drain accumulator =
    match pull stream with
    | None -> (List.rev accumulator, get_location (), List.rev !reports)
    | Some scalar -> drain (scalar :: accumulator)
  in
  drain []

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
      (Printf.sprintf "expected 840 HTML files under %s, found %d" root
         (List.length files));
  List.iteri
    (fun index path ->
      try
        let scalars, eof_location, _preprocessing_reports =
          read_file path |> preprocess
        in
        let termination, _parser_reports =
          shadow_parse None ~eof_location scalars
        in
        match termination with
        | `EOF ->
            if (index + 1) mod 50 = 0 then
              Printf.eprintf "checked %d/840\r%!" (index + 1)
        | `Exception message ->
            failwith (Printf.sprintf "parser exception: %s" message)
        | `Timeout -> failwith "parser did not terminate"
      with exn ->
        Printf.eprintf "\nTokenizer mismatch in %s: %s\n%!" path
          (Printexc.to_string exn);
        exit 1)
    files;
  Printf.printf "Compared reference and candidate tokenizers on 840 files.\n"
