type result = { count : int; hash : int64; retained : Obj.t option }

let fnv_offset = -3750763034362895579L
let fnv_prime = 1099511628211L

let hash_bytes hash bytes =
  let hash = ref hash in
  String.iter
    (fun byte ->
      hash := Int64.logxor !hash (Int64.of_int (Char.code byte));
      hash := Int64.mul !hash fnv_prime)
    bytes;
  !hash

let hash_value hash value =
  hash_bytes hash (Marshal.to_string value [ Marshal.No_sharing ])

let html_files directory =
  let files = CCIO.File.read_dir ~recurse:true (CCIO.File.make directory) in
  let rec collect accumulator =
    match files () with
    | None -> List.sort String.compare accumulator
    | Some path ->
        let path = CCIO.File.to_string path in
        let accumulator =
          if Filename.check_suffix path ".html" then path :: accumulator
          else accumulator
        in
        collect accumulator
  in
  collect []

let report _location _error _throw resume = resume ()

let tokenizer body =
  let input, get_location =
    body |> Markup__Stream_io.string
    |> Markup__Encoding.utf_8 ~report
    |> Markup__Input.preprocess Markup__Common.is_valid_html_char report
  in
  Markup__Html_tokenizer.create_pull report (input, get_location)

let count_tokens materialize bodies =
  let count = ref 0 in
  let hash = ref fnv_offset in
  let retained = ref [] in
  List.iter
    (fun body ->
      let tokenizer = tokenizer body in
      let location = Markup__Html_tokenizer.{ line = 1; column = 1 } in
      let rec loop () =
        let token = Markup__Html_tokenizer.next tokenizer location in
        incr count;
        hash := hash_value !hash ((location.line, location.column), token);
        if materialize then
          retained := ((location.line, location.column), token) :: !retained;
        if token <> `EOF then loop ()
      in
      loop ())
    bodies;
  {
    count = !count;
    hash = !hash;
    retained =
      (if materialize then Some (Obj.repr (Sys.opaque_identity !retained))
       else None);
  }

let baseline_parse bodies =
  let count = ref 0 in
  let hash = ref fnv_offset in
  List.iter
    (fun body ->
      body |> Markup.string |> Markup.parse_html |> Markup.signals
      |> Markup.iter (fun signal ->
          incr count;
          hash := hash_value !hash signal))
    bodies;
  { count = !count; hash = !hash; retained = None }

let lite_parse bodies =
  let count = ref 0 in
  let hash = ref fnv_offset in
  List.iter
    (fun body ->
      body |> Markup_lite.parse_html
      |> Markup_lite.iter (fun signal ->
          incr count;
          hash := hash_value !hash signal))
    bodies;
  { count = !count; hash = !hash; retained = None }

let peak_rss_kib () =
  try
    let channel = open_in "/proc/self/status" in
    let rec find () =
      match input_line channel with
      | line when String.starts_with ~prefix:"VmHWM:" line ->
          close_in channel;
          Scanf.sscanf line "VmHWM: %d kB" Option.some
      | _ -> find ()
      | exception End_of_file ->
          close_in channel;
          None
    in
    find ()
  with Sys_error _ -> None

let measure bytes name run =
  Gc.full_major ();
  let gc_before = Gc.quick_stat () in
  let cpu_before = Unix.times () in
  let wall_before = Unix.gettimeofday () in
  let result = run () in
  let wall = Unix.gettimeofday () -. wall_before in
  let cpu_after = Unix.times () in
  let gc_after = Gc.quick_stat () in
  let mib = Float.of_int bytes /. 1048576. in
  let rss =
    match peak_rss_kib () with None -> "n/a" | Some kib -> string_of_int kib
  in
  Printf.printf
    "%s wall=%.6f user=%.6f throughput_mib_s=%.2f count=%d hash=%016Lx \
     minor_words=%.0f major_words=%.0f minor_collections=%d \
     major_collections=%d live_words=%d peak_rss_kib=%s\n\
     %!"
    name wall
    (cpu_after.tms_utime -. cpu_before.tms_utime)
    (mib /. wall) result.count result.hash
    (gc_after.minor_words -. gc_before.minor_words)
    (gc_after.major_words -. gc_before.major_words)
    (gc_after.minor_collections - gc_before.minor_collections)
    (gc_after.major_collections - gc_before.major_collections)
    gc_after.live_words rss;
  ignore (Sys.opaque_identity result.retained)

let () =
  let directory =
    if Array.length Sys.argv = 2 then Sys.argv.(1)
    else begin
      Printf.eprintf "Usage: %s DIRECTORY\n" Sys.argv.(0);
      exit 2
    end
  in
  let paths = html_files directory in
  let bodies = List.map CCIO.File.read_exn paths in
  let bytes =
    List.fold_left (fun total body -> total + String.length body) 0 bodies
  in
  Printf.printf "files=%d bytes=%d\n%!" (List.length bodies) bytes;
  measure bytes "tokenizer_count" (fun () -> count_tokens false bodies);
  measure bytes "tokenizer_materialized" (fun () -> count_tokens true bodies);
  measure bytes "baseline_parse" (fun () -> baseline_parse bodies);
  measure bytes "lite_parse" (fun () -> lite_parse bodies)
