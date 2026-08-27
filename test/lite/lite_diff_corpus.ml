let usage program =
  Printf.eprintf "Usage: %s DIRECTORY\n" program;
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

let collect iter stream =
  let values = ref [] in
  iter (fun value -> values := value :: !values) stream;
  List.rev !values

let equal_lists equal left right =
  let rec loop left right =
    match (left, right) with
    | [], [] -> true
    | l :: ls, r :: rs when equal l r -> loop ls rs
    | _ -> false
  in
  loop left right

let first_difference to_string left right =
  let rec loop index left right =
    match (left, right) with
    | [], [] -> "no difference"
    | [], _ :: _ -> Printf.sprintf "left ended at signal %d" index
    | _ :: _, [] -> Printf.sprintf "right ended at signal %d" index
    | l :: ls, r :: rs ->
        if l = r then loop (index + 1) ls rs
        else
          Printf.sprintf "signal %d:\n  oracle: %s\n  lite:   %s" index
            (to_string l) (to_string r)
  in
  loop 0 left right

type result = Parsed of Markup_common.signal list | Raised of string
type stats = { mutable wall_seconds : float; mutable minor_words : float }

let empty_stats () = { wall_seconds = 0.; minor_words = 0. }

let measure stats f =
  let minor_words_before = (Gc.quick_stat ()).minor_words in
  let wall_before = Unix.gettimeofday () in
  let result = f () in
  stats.wall_seconds <-
    stats.wall_seconds +. Unix.gettimeofday () -. wall_before;
  stats.minor_words <-
    stats.minor_words +. (Gc.quick_stat ()).minor_words -. minor_words_before;
  result

let run parse collect_signals html =
  let report _ _ = () in
  try Parsed (collect_signals (parse report html))
  with exn -> Raised (Printexc.to_string exn)

let compare path oracle lite =
  match (oracle, lite) with
  | Parsed oracle_signals, Parsed lite_signals ->
      if not (equal_lists ( = ) oracle_signals lite_signals) then begin
        Printf.eprintf "%s: signal mismatch: %s\n" path
          (first_difference Markup_common.signal_to_string oracle_signals
             lite_signals);
        false
      end
      else true
  | Raised oracle_exn, Raised lite_exn ->
      if oracle_exn = lite_exn then true
      else begin
        Printf.eprintf "%s: exception mismatch:\n  oracle: %s\n  lite:   %s\n"
          path oracle_exn lite_exn;
        false
      end
  | Raised exn, Parsed _ ->
      Printf.eprintf "%s: oracle raised but Lite did not: %s\n" path exn;
      false
  | Parsed _, Raised exn ->
      Printf.eprintf "%s: Lite raised but oracle did not: %s\n" path exn;
      false

let check_case name html =
  let oracle = run Oracle.parse (collect Markup.iter) html in
  let lite =
    run
      (fun report html -> Markup_lite.parse_html ~report html)
      (collect Markup_lite.iter) html
  in
  if not (compare name oracle lite) then exit 1

let check_decoder_cases () =
  [
    ("decoder empty", "");
    ("decoder ASCII without ampersand", "<p>plain ASCII text</p>");
    ("decoder UTF-8 without ampersand", "<p title=\"été 東京\">naïve ✓</p>");
    ( "decoder attributes without ampersand",
      "<div data-value=\"${value}\">x</div>" );
    ("decoder complete references", "<p title=\"a&amp;b\">&lt;&#62;&#x3e;</p>");
    ("decoder incomplete references", "<p>a& b&amp b&#12 b&#x2a</p>");
  ]
  |> List.iter (fun (name, html) -> check_case name html)

let check_depth_limit () =
  let html = "<div><span></div>" in
  let oracle = run (Oracle.parse ~depth_limit:1) (collect Markup.iter) html in
  let lite =
    run
      (fun report html -> Markup_lite.parse_html ~report ~depth_limit:1 html)
      (collect Markup_lite.iter) html
  in
  match (oracle, lite) with
  | Raised _, Raised _ ->
      if not (compare "depth-limit check" oracle lite) then exit 1
  | _ ->
      Printf.eprintf "depth-limit check: expected both parsers to raise\n";
      exit 1

let () =
  check_decoder_cases ();
  check_depth_limit ();
  let directory =
    match Array.to_list Sys.argv with
    | [ _; directory ] -> directory
    | _ -> usage Sys.argv.(0)
  in
  let files = html_files directory in
  if files = [] then begin
    Printf.eprintf "No .html files found under: %s\n" directory;
    exit 2
  end;
  let failures = ref 0 in
  let oracle_stats = empty_stats () in
  let lite_stats = empty_stats () in
  List.iteri
    (fun index path ->
      let html = CCIO.File.read_exn (CCIO.File.make path) in
      let oracle =
        measure oracle_stats (fun () ->
            run Oracle.parse (collect Markup.iter) html)
      in
      let lite =
        measure lite_stats (fun () ->
            run
              (fun report html -> Markup_lite.parse_html ~report html)
              (collect Markup_lite.iter) html)
      in
      if not (compare path oracle lite) then incr failures;
      if (index + 1) mod 100 = 0 then
        Printf.eprintf "checked %d/%d\r%!" (index + 1) (List.length files))
    files;
  Printf.eprintf "checked %d/%d\n%!" (List.length files) (List.length files);
  Printf.printf
    "oracle: wall_seconds=%.6f minor_words=%.0f\n\
     lite:   wall_seconds=%.6f minor_words=%.0f\n\
     %!"
    oracle_stats.wall_seconds oracle_stats.minor_words lite_stats.wall_seconds
    lite_stats.minor_words;
  if !failures <> 0 then begin
    Printf.eprintf "FAILED: %d files differed\n" !failures;
    exit 1
  end;
  Printf.printf "OK: %d HTML files matched exactly\n%!" (List.length files)
