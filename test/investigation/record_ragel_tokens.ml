module Tokenizer = Test_support.Ragel_html_tokenizer
module Snapshot = Test_support.Ragel_token_snapshot

let usage program =
  Printf.eprintf "Usage: %s OUTPUT CORPUS REGRESSION_INPUTS\n" program;
  exit 2

let rec files_under directory =
  Sys.readdir directory |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun name ->
         let path = Filename.concat directory name in
         if Sys.is_directory path then files_under path else [ path ])

let record_corpus output directory =
  let files =
    files_under directory
    |> List.filter (fun path -> Filename.check_suffix path ".html")
  in
  List.iteri
    (fun index path ->
      let channel = open_in_bin path in
      let length = in_channel_length channel in
      let input = really_input_string channel length in
      close_in channel;
      let snapshot : Snapshot.t =
        { source = "corpus:" ^ path; input = None; tokens = Tokenizer.tokenize input }
      in
      Marshal.to_channel output snapshot [ Marshal.No_sharing ];
      if (index + 1) mod 100 = 0 then
        Printf.eprintf "recorded corpus %d/%d\r%!" (index + 1)
          (List.length files))
    files;
  Printf.eprintf "recorded corpus %d/%d\n%!" (List.length files)
    (List.length files)

let record_regressions output directory =
  let files = files_under directory in
  List.iter
    (fun path ->
      let channel = open_in_bin path in
      let length = in_channel_length channel in
      let input = really_input_string channel length in
      close_in channel;
      let snapshot : Snapshot.t =
        {
          source = "regression:" ^ Filename.basename path;
          input = Some input;
          tokens = Tokenizer.tokenize input;
        }
      in
      Marshal.to_channel output snapshot [ Marshal.No_sharing ])
    files;
  Printf.eprintf "recorded regression literals: %d\n%!" (List.length files)

let () =
  match Array.to_list Sys.argv with
  | [ _; output_path; corpus; regressions ] ->
      let output = open_out_bin output_path in
      output_string output "MARKUP-RAGEL-TOKENS-1\n";
      record_corpus output corpus;
      record_regressions output regressions;
      close_out output
  | _ -> usage Sys.argv.(0)
