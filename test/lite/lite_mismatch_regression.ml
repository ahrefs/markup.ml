(* Representative non-NUL mismatch families from some production mismatches.
   The production path first adapts HtmlStream tokens, so these tests use the
   same adapter rather than parsing independently tokenized HTML. *)

open OUnit2

let collect iter stream =
  let signals = ref [] in
  iter (fun signal -> signals := signal :: !signals) stream;
  List.rev !signals

let baseline html =
  let tokens = Oracle.adapt html in
  collect Markup.iter
    (Oracle.parse_adapted ~context:`Document (fun _ _ -> ()) tokens)

let lite html =
  let tokens = Oracle.adapt html in
  collect Markup_lite.iter
    (Oracle.parse_lite_adapted ~context:`Document (fun _ _ -> ()) tokens)

let print_signals signals =
  signals
  |> List.map Markup_common.signal_to_string
  |> String.concat "\n  " |> Printf.sprintf "\n  %s"

let agrees name html =
  name >:: fun _ ->
  assert_equal ~printer:print_signals (baseline html) (lite html)

let () =
  run_test_tt_main
    ("production mismatch regressions"
    >::: [
           (* Representative of the first-diff-965 gachon.ac.kr pages. *)
           agrees "paragraph reconstruction across an active anchor"
             "<p><a href='https://example.test/'>one<p>two";
           (* Representative of the first-diff-884/885 gachon.ac.kr pages. *)
           agrees "nested bold reconstruction across a block"
             "<div><b><b><div>text";
           (* Representative of the first-diff-992/998 toyless pages. *)
           agrees "font reconstruction inside a nested div"
             "<div class='options'><font color='#138f9a'><div id='option'>text";
           (* Minimized from http://koganeijinja.com/: the trailing newline is
              required. HtmlStream presents it as a String token. *)
           agrees "formatting reconstruction after stripped pre newline"
             "<p><font><pre>\n";
           agrees "formatting reconstruction after stripped listing newline"
             "<p><font><listing>\n";
         ])
