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

exception Timeout

type result =
  | Parsed of
      Markup_common.signal list
      * (Markup_common.location * Markup_common.Error.t) list
  | Raised of string * (Markup_common.location * Markup_common.Error.t) list

let with_timeout f =
  let previous =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout))
  in
  ignore (Unix.alarm 2);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm previous)
    f

let run parse collect_signals =
  let errors = ref [] in
  let report location error = errors := (location, error) :: !errors in
  try
    Parsed
      (with_timeout (fun () -> collect_signals (parse report)), List.rev !errors)
  with exn -> Raised (Printexc.to_string exn, List.rev !errors)

let first_difference to_string left right =
  let rec loop index left right =
    match (left, right) with
    | [], [] -> "no difference"
    | [], _ :: _ -> Printf.sprintf "baseline ended at %d" index
    | _ :: _, [] -> Printf.sprintf "Lite ended at %d" index
    | x :: xs, y :: ys ->
        if x = y then loop (index + 1) xs ys
        else
          Printf.sprintf "item %d: baseline=%s Lite=%s" index (to_string x)
            (to_string y)
  in
  loop 0 left right

let error_to_string ((line, column), error) =
  Printf.sprintf "(%d,%d) %s" line column (Markup_common.Error.to_string error)

let compare name baseline lite =
  if baseline = lite then true
  else begin
    begin match (baseline, lite) with
    | Parsed (signals, errors), Parsed (lite_signals, lite_errors) ->
        if signals <> lite_signals then
          Printf.eprintf "%s: signal mismatch: %s\n" name
            (first_difference Markup_common.signal_to_string signals
               lite_signals)
        else
          Printf.eprintf "%s: error mismatch: %s\n" name
            (first_difference error_to_string errors lite_errors)
    | Raised (exn, _), Raised (lite_exn, _) ->
        Printf.eprintf "%s: exception mismatch: baseline=%S Lite=%S\n" name exn
          lite_exn
    | Raised (exn, _), Parsed _ ->
        Printf.eprintf "%s: baseline raised %S but Lite parsed\n" name exn
    | Parsed _, Raised (exn, _) ->
        Printf.eprintf "%s: Lite raised %S but baseline parsed\n" name exn
    end;
    false
  end

let contexts =
  [
    ("document", `Document);
    ("div-fragment", `Fragment "div");
    ("table-fragment", `Fragment "table");
    ("svg-fragment", `Fragment "svg");
    ("math-fragment", `Fragment "math");
  ]

let check path html =
  (* Adapt exactly once. Both parsers receive values derived from this list. *)
  let tokens = Oracle.adapt html in
  List.for_all
    (fun (context_name, context) ->
      List.for_all
        (fun depth_limit ->
          let suffix =
            match depth_limit with
            | None -> context_name
            | Some n -> Printf.sprintf "%s/depth-%d" context_name n
          in
          let baseline =
            run
              (fun report ->
                Oracle.parse_adapted ?depth_limit ~context report tokens)
              (collect Markup.iter)
          in
          let lite =
            run
              (fun report ->
                Oracle.parse_lite_adapted ?depth_limit ~context report tokens)
              (collect Markup_lite.iter)
          in
          compare (path ^ ":" ^ suffix) baseline lite)
        [ None; Some 1; Some 8 ])
    contexts

let () =
  let directory =
    match Array.to_list Sys.argv with
    | [ _; directory ] -> directory
    | _ -> usage Sys.argv.(0)
  in
  let files = html_files directory in
  if files = [] then usage Sys.argv.(0);
  let failures = ref 0 in
  List.iteri
    (fun index path ->
      if not (check path (CCIO.File.read_exn (CCIO.File.make path))) then
        incr failures;
      if (index + 1) mod 100 = 0 then
        Printf.eprintf "checked %d/%d\r%!" (index + 1) (List.length files))
    files;
  Printf.eprintf "checked %d/%d\n%!" (List.length files) (List.length files);
  if !failures <> 0 then begin
    Printf.eprintf "FAILED: %d files differed\n" !failures;
    exit 1
  end
