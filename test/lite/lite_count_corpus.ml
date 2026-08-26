type parser_selection = Both | Oracle | Lite

let arguments () =
  let selection = ref Both in
  let directories = ref [] in
  let parser = function
    | "both" -> selection := Both
    | "oracle" -> selection := Oracle
    | "lite" -> selection := Lite
    | _ -> assert false
  in
  let usage =
    Printf.sprintf "Usage: %s [--parser both|oracle|lite] DIRECTORY"
      Sys.argv.(0)
  in
  let specs =
    [ ( "--parser",
        Arg.Symbol ([ "both"; "oracle"; "lite" ], parser),
        " Select which parser to run (default: both)" ) ]
  in
  Arg.parse specs
    (fun directory -> directories := directory :: !directories)
    usage;
  match !directories with
  | [ directory ] -> (!selection, directory)
  | _ ->
      Arg.usage specs usage;
      exit 2

let html_files directory =
  let files = CCIO.File.read_dir ~recurse:true (CCIO.File.make directory) in
  let rec collect acc =
    match files () with
    | None -> List.sort String.compare acc
    | Some path ->
        let path = CCIO.File.to_string path in
        collect
          (if Filename.check_suffix path ".html" then path :: acc else acc)
  in
  collect []

type stats = {
  mutable wall_seconds : float;
  mutable minor_words : float;
  mutable major_words : float;
  mutable promoted_words : float;
}

let empty_stats () =
  { wall_seconds = 0.; minor_words = 0.; major_words = 0.; promoted_words = 0. }

let measure stats f =
  let gc_before = Gc.quick_stat () in
  let wall_before = Unix.gettimeofday () in
  let result = f () in
  let wall_after = Unix.gettimeofday () in
  let gc_after = Gc.quick_stat () in
  stats.wall_seconds <- stats.wall_seconds +. wall_after -. wall_before;
  stats.minor_words <-
    stats.minor_words +. gc_after.minor_words -. gc_before.minor_words;
  stats.major_words <-
    stats.major_words +. gc_after.major_words -. gc_before.major_words;
  stats.promoted_words <-
    stats.promoted_words +. gc_after.promoted_words -. gc_before.promoted_words;
  result

type outcome = Count of int | Raised of string

let count iter stream =
  let count = ref 0 in
  iter (fun _ -> incr count) stream;
  !count

let run f = try Count (f ()) with exn -> Raised (Printexc.to_string exn)

let count_oracle html =
  run (fun () -> Oracle.parse (fun _ _ -> ()) html |> count Markup.iter)

let count_lite html =
  run (fun () -> Markup_lite.parse_html html |> count Markup_lite.iter)

let compare path oracle lite =
  match (oracle, lite) with
  | Count oracle, Count lite when oracle = lite -> Some oracle
  | Raised oracle, Raised lite when oracle = lite -> Some 0
  | Count oracle, Count lite ->
      Printf.eprintf "%s: signal count differs: oracle=%d lite=%d\n" path oracle
        lite;
      None
  | Raised oracle, Raised lite ->
      Printf.eprintf "%s: exception differs:\n  oracle: %s\n  lite:   %s\n" path
        oracle lite;
      None
  | Raised exception_, Count count ->
      Printf.eprintf "%s: oracle raised but Lite produced %d signals: %s\n" path
        count exception_;
      None
  | Count count, Raised exception_ ->
      Printf.eprintf "%s: Lite raised but oracle produced %d signals: %s\n" path
        count exception_;
      None

let print_stats name stats =
  Printf.printf
    "%s: wall_seconds=%.6f minor_words=%.0f major_words=%.0f promoted_words=%.0f\n"
    name stats.wall_seconds stats.minor_words stats.major_words
    stats.promoted_words

let run_one name count_parser files =
  let stats = empty_stats () in
  let bytes = ref 0 in
  let signals = ref 0 in
  let exceptions = ref 0 in
  List.iteri
    (fun index path ->
      let html = CCIO.File.read_exn (CCIO.File.make path) in
      bytes := !bytes + String.length html;
      begin match measure stats (fun () -> count_parser html) with
      | Count count -> signals := !signals + count
      | Raised _ -> incr exceptions
      end;
      if (index + 1) mod 100 = 0 then
        Printf.eprintf "checked %d/%d\r%!" (index + 1) (List.length files))
    files;
  Printf.eprintf "checked %d/%d\n%!" (List.length files) (List.length files);
  Printf.printf "files=%d bytes=%d signals=%d exceptions=%d\n"
    (List.length files) !bytes !signals !exceptions;
  print_stats (name ^ " count") stats;
  Printf.printf "OK: %d HTML files consumed with %s\n%!" (List.length files)
    name

let run_both files =
  let failures = ref 0 in
  let bytes = ref 0 in
  let signals = ref 0 in
  let oracle_stats = empty_stats () in
  let lite_stats = empty_stats () in
  List.iteri
    (fun index path ->
      let html = CCIO.File.read_exn (CCIO.File.make path) in
      bytes := !bytes + String.length html;
      let oracle, lite =
        if index mod 2 = 0 then begin
          let oracle = measure oracle_stats (fun () -> count_oracle html) in
          let lite = measure lite_stats (fun () -> count_lite html) in
          (oracle, lite)
        end
        else begin
          let lite = measure lite_stats (fun () -> count_lite html) in
          let oracle = measure oracle_stats (fun () -> count_oracle html) in
          (oracle, lite)
        end
      in
      begin match compare path oracle lite with
      | Some count -> signals := !signals + count
      | None -> incr failures
      end;
      if (index + 1) mod 100 = 0 then
        Printf.eprintf "checked %d/%d\r%!" (index + 1) (List.length files))
    files;
  Printf.eprintf "checked %d/%d\n%!" (List.length files) (List.length files);
  Printf.printf "files=%d bytes=%d signals=%d\n" (List.length files) !bytes
    !signals;
  print_stats "oracle count" oracle_stats;
  print_stats "lite count" lite_stats;
  if !failures <> 0 then begin
    Printf.eprintf "FAILED: %d files differed\n" !failures;
    exit 1
  end;
  Printf.printf "OK: %d HTML files had equal signal counts\n%!"
    (List.length files)

let () =
  let selection, directory = arguments () in
  let files = html_files directory in
  if files = [] then begin
    Printf.eprintf "No .html files found under: %s\n" directory;
    exit 2
  end;
  match selection with
  | Both -> run_both files
  | Oracle -> run_one "oracle" count_oracle files
  | Lite -> run_one "lite" count_lite files
