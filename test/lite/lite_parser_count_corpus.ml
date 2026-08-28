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

let count iter stream =
  let count = ref 0 in
  iter (fun _ -> incr count) stream;
  !count

type stats = {
  mutable wall : float;
  mutable user : float;
  mutable system : float;
  mutable words : float;
  mutable minor_collections : int;
  mutable major_collections : int;
}

let stats () =
  {
    wall = 0.;
    user = 0.;
    system = 0.;
    words = 0.;
    minor_collections = 0;
    major_collections = 0;
  }

let measure stats f =
  let gc_before = Gc.quick_stat () in
  let cpu_before = Unix.times () in
  let wall_before = Unix.gettimeofday () in
  let result = f () in
  let wall_after = Unix.gettimeofday () in
  let cpu_after = Unix.times () in
  let gc_after = Gc.quick_stat () in
  stats.wall <- stats.wall +. wall_after -. wall_before;
  stats.user <- stats.user +. cpu_after.tms_utime -. cpu_before.tms_utime;
  stats.system <- stats.system +. cpu_after.tms_stime -. cpu_before.tms_stime;
  stats.words <-
    stats.words +. gc_after.minor_words +. gc_after.major_words
    -. gc_before.minor_words -. gc_before.major_words;
  stats.minor_collections <-
    stats.minor_collections + gc_after.minor_collections
    - gc_before.minor_collections;
  stats.major_collections <-
    stats.major_collections + gc_after.major_collections
    - gc_before.major_collections;
  result

let print name bytes stats =
  Printf.printf
    "%s: wall_seconds=%.6f user_seconds=%.6f system_seconds=%.6f \
     throughput_mib_s=%.2f allocated_words=%.0f minor_collections=%d \
     major_collections=%d\n"
    name stats.wall stats.user stats.system
    (float bytes /. 1048576. /. stats.wall)
    stats.words stats.minor_collections stats.major_collections

let () =
  let directory =
    match Array.to_list Sys.argv with
    | [ _; directory ] -> directory
    | _ -> usage Sys.argv.(0)
  in
  let files = html_files directory in
  if files = [] then usage Sys.argv.(0);
  let baseline_stats = stats () in
  let lite_stats = stats () in
  let bytes = ref 0 in
  let failures = ref 0 in
  List.iteri
    (fun index path ->
      let html = CCIO.File.read_exn (CCIO.File.make path) in
      bytes := !bytes + String.length html;
      (* HtmlStream production and adaptation are outside both timed regions. *)
      let tokens = Oracle.adapt html in
      let baseline () =
        Oracle.parse_adapted ~context:`Document (fun _ _ -> ()) tokens
        |> count Markup.iter
      in
      let lite () =
        Oracle.parse_lite_adapted ~context:`Document (fun _ _ -> ()) tokens
        |> count Markup_lite.iter
      in
      let baseline_count, lite_count =
        if index mod 2 = 0 then
          (measure baseline_stats baseline, measure lite_stats lite)
        else
          let lite_count = measure lite_stats lite in
          (measure baseline_stats baseline, lite_count)
      in
      if baseline_count <> lite_count then begin
        incr failures;
        Printf.eprintf "%s: signal count differs: %d vs %d\n" path
          baseline_count lite_count
      end)
    files;
  print "baseline parser-only" !bytes baseline_stats;
  print "Lite parser-only" !bytes lite_stats;
  if !failures <> 0 then exit 1
