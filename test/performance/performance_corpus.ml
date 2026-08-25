(* Parse a directory tree of HTML files and count links. *)

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
      let acc = if Filename.check_suffix path ".html" then path :: acc else acc in
      collect acc
  in
  collect []

let parse_file path =
  let body = CCIO.File.read_exn (CCIO.File.make path) in
  let links = ref 0 in
  body
  |> Markup.string
  |> Markup.parse_html ~context:`Document
  |> Markup.signals
  |> Markup.iter (function
       | `Start_element ((_, "a"), _) -> incr links
       | _ -> ());
  String.length body, !links

let () =
  let directory =
    match Array.to_list Sys.argv with
    | [_; directory] -> directory
    | _ -> usage Sys.argv.(0)
  in
  let directory_file = CCIO.File.make directory in
  if not (CCIO.File.exists directory_file) then begin
    Printf.eprintf "Directory does not exist: %s\n" directory;
    exit 2
  end;
  if not (CCIO.File.is_directory directory_file) then begin
    Printf.eprintf "Not a directory: %s\n" directory;
    exit 2
  end;

  let started = Unix.gettimeofday () in
  let files = html_files directory in
  if files = [] then begin
    Printf.eprintf "No .html files found under: %s\n" directory;
    exit 2
  end;

  let bytes, links =
    List.fold_left
      (fun (total_bytes, total_links) path ->
        let bytes, links = parse_file path in
        total_bytes + bytes, total_links + links)
      (0, 0) files
  in
  let elapsed = Unix.gettimeofday () -. started in
  let mib = Float.of_int bytes /. (1024. *. 1024.) in
  Printf.printf
    "files=%d bytes=%d a_tags=%d wall_seconds=%.6f throughput_mib_s=%.2f\n%!"
    (List.length files) bytes links elapsed (mib /. elapsed)
