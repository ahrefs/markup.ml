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

let collect stream =
  let signals = ref [] in
  Markup_lite.iter (fun signal -> signals := signal :: !signals) stream;
  List.rev !signals

type stats = {
  mutable wall_seconds : float;
  mutable minor_words : float;
  mutable major_words : float;
}

let empty_stats () = { wall_seconds = 0.; minor_words = 0.; major_words = 0. }

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
  result

let write_with_markup signals =
  signals |> Markup.of_list |> Markup.write_html |> Markup.to_string

let write_with_lite signals =
  let buffer = Buffer.create 4096 in
  Markup_lite.write_html buffer (Markup.of_list signals);
  Buffer.contents buffer

let check_escape_regressions () =
  let cases =
    [
      ("ASCII-safe", [ `Text [ "plain text" ] ]);
      ("ASCII escapes", [ `Text [ "<&>" ] ]);
      ( "attribute escapes",
        [
          `Start_element ((Markup.Ns.html, "p"), [ (("", "title"), "a&\"b") ]);
          `End_element;
        ] );
      ("UTF-8 and non-breaking space", [ `Text [ "été\xC2\xA0東京" ] ]);
      ("malformed UTF-8", [ `Text [ "before\xFFafter" ] ]);
    ]
  in
  List.iter
    (fun (name, signals) ->
      let markup = write_with_markup signals in
      let lite = write_with_lite signals in
      if markup <> lite then begin
        Printf.eprintf "%s escape regression:\n  Markup: %S\n  Lite:   %S\n"
          name markup lite;
        exit 1
      end)
    cases

let check_buffer_api () =
  let wrap prefix buffer text =
    Buffer.add_string buffer prefix;
    Buffer.add_char buffer '(';
    Buffer.add_string buffer text;
    Buffer.add_char buffer ')'
  in
  let signals =
    [
      `Start_element ((Markup.Ns.html, "p"), [ (("", "id"), "x") ]);
      `Text [ "y" ];
      `End_element;
    ]
  in
  let buffer = Buffer.create 64 in
  Buffer.add_string buffer "prefix:";
  Markup_lite.write_html ~escape_attribute:(wrap "A") ~escape_text:(wrap "T")
    buffer (Markup.of_list signals);
  Markup_lite.write_html buffer (Markup.of_list [ `Text [ "<&" ] ]);
  let expected = "prefix:<p id=\"A(x)\">T(y)</p>&lt;&amp;" in
  if Buffer.contents buffer <> expected then begin
    Printf.eprintf "buffer API check failed:\n  expected: %S\n  actual:   %S\n"
      expected (Buffer.contents buffer);
    exit 1
  end

let difference left right =
  let left_length = String.length left in
  let right_length = String.length right in
  let limit = min left_length right_length in
  let rec find index =
    if index = limit then index
    else if left.[index] = right.[index] then find (index + 1)
    else index
  in
  let index = find 0 in
  let context string =
    let start = max 0 (index - 24) in
    let length = min (String.length string - start) 64 in
    String.sub string start length
  in
  if index = limit then
    Printf.sprintf
      "output lengths differ at byte %d (Markup: %d, Lite: %d)\n\
      \  Markup: %S\n\
      \  Lite:   %S"
      index left_length right_length (context left) (context right)
  else
    Printf.sprintf
      "output differs at byte %d (Markup: 0x%02X, Lite: 0x%02X)\n\
      \  Markup: %S\n\
      \  Lite:   %S"
      index
      (Char.code left.[index])
      (Char.code right.[index])
      (context left) (context right)

let () =
  check_escape_regressions ();
  check_buffer_api ();
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
  let markup_stats = empty_stats () in
  let lite_stats = empty_stats () in
  List.iteri
    (fun index path ->
      let html = CCIO.File.read_exn (CCIO.File.make path) in
      let signals = collect (Markup_lite.parse_html html) in
      let markup, lite =
        if index mod 2 = 0 then begin
          let markup =
            measure markup_stats (fun () -> write_with_markup signals)
          in
          let lite = measure lite_stats (fun () -> write_with_lite signals) in
          (markup, lite)
        end
        else begin
          let lite = measure lite_stats (fun () -> write_with_lite signals) in
          let markup =
            measure markup_stats (fun () -> write_with_markup signals)
          in
          (markup, lite)
        end
      in
      if markup <> lite then begin
        incr failures;
        Printf.eprintf "%s: %s\n" path (difference markup lite)
      end;
      if (index + 1) mod 100 = 0 then
        Printf.eprintf "checked %d/%d\r%!" (index + 1) (List.length files))
    files;
  Printf.eprintf "checked %d/%d\n%!" (List.length files) (List.length files);
  Printf.printf
    "markup writer: wall_seconds=%.6f minor_words=%.0f major_words=%.0f\n\
     lite writer:   wall_seconds=%.6f minor_words=%.0f major_words=%.0f\n\
     %!"
    markup_stats.wall_seconds markup_stats.minor_words markup_stats.major_words
    lite_stats.wall_seconds lite_stats.minor_words lite_stats.major_words;
  if !failures <> 0 then begin
    Printf.eprintf "FAILED: %d files differed\n" !failures;
    exit 1
  end;
  Printf.printf "OK: %d HTML files serialized exactly\n%!" (List.length files)
