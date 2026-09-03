(* Regression cases minimized from the 2026-08-27 Lite differential fuzzing
   corpus. The devkit-backed parser is the oracle for all of these cases. *)

open OUnit2

let collect iter stream =
  let signals = ref [] in
  iter (fun signal -> signals := signal :: !signals) stream;
  List.rev !signals

let oracle html = collect Markup.iter (Oracle.parse (fun _ _ -> ()) html)

let lite html =
  collect Markup_lite.iter (Markup_lite.parse_html ~report:(fun _ _ -> ()) html)

let print_signals signals =
  signals
  |> List.map Markup_common.signal_to_string
  |> String.concat "\n  " |> Printf.sprintf "\n  %s"

let agrees name html =
  name >:: fun _ ->
  assert_equal ~printer:print_signals (oracle html) (lite html)

let adapted_oracle html =
  let tokens = Oracle.adapt html in
  collect Markup.iter
    (Oracle.parse_adapted ~context:`Document (fun _ _ -> ()) tokens)

let adapted_lite html =
  let tokens = Oracle.adapt html in
  collect Markup_lite.iter
    (Oracle.parse_lite_adapted ~context:`Document (fun _ _ -> ()) tokens)

let adapted_agrees name html =
  name >:: fun _ ->
  assert_equal ~printer:print_signals (adapted_oracle html) (adapted_lite html)

(* [adapted_agrees] compares signals only. These two also need the error
   stream: the 2026-09-03 divergences below were error-only or error-visible. *)
let adapted_outcome parse html =
  let tokens = Oracle.adapt html in
  let errors = ref [] in
  let report location error = errors := (location, error) :: !errors in
  let signals = parse report tokens in
  ( List.map Markup_common.signal_to_string signals,
    List.rev_map
      (fun ((line, column), error) ->
        Printf.sprintf "(%d,%d) %s" line column
          (Markup_common.Error.to_string error))
      !errors )

let adapted_oracle_outcome =
  adapted_outcome (fun report tokens ->
      collect Markup.iter
        (Oracle.parse_adapted ~context:`Document report tokens))

let adapted_lite_outcome =
  adapted_outcome (fun report tokens ->
      collect Markup_lite.iter
        (Oracle.parse_lite_adapted ~context:`Document report tokens))

let print_outcome (signals, errors) =
  Printf.sprintf "\n  %s" (String.concat "\n  " (signals @ errors))

let adapted_agrees_with_errors name html =
  name >:: fun _ ->
  assert_equal ~printer:print_outcome
    (adapted_oracle_outcome html)
    (adapted_lite_outcome html)

let lite_parses name html = name >:: fun _ -> ignore (lite html)

let rawtext_failures =
  [
    ("garbage script at EOF", "<script/;>");
    ("garbage script with content at EOF", "<script//>x");
    ("garbage script with close tag", "<script/x>alert(1)</script>done");
    ("garbage title", "<title/x>a rest</title>done");
    ("garbage style", "<style/x>p{}</style>done");
    ("garbage in attribute position (/=)", "<script /=>x</script>y");
    ("garbage in attribute position (=)", "<script =>x</script>y");
    ("garbage after retained attribute", "<script a/b>x</script>y");
    ("less-than inside rawtext start tag", "ex<script\001more<b>");
  ]
  |> List.map (fun (name, html) -> agrees name html)

let rawtext_guards =
  [
    ("non-rawtext slash garbage", "<b/x>text");
    ("non-rawtext less-than garbage", "<div foo<bar>text");
    ("rawtext garbage after valued attribute", "<script foo=1//>x");
    ("valid self-closing script", "<script />x");
    ("normal rawtext EOF", "<script>x");
    ("control character is tag whitespace", "<scripT\001more>x");
  ]
  |> List.map (fun (name, html) -> agrees name html)

let entity_failures =
  [
    ("text surrogate numeric reference", "x&#xdeee;y");
    ("text first surrogate numeric reference", "x&#xd800;");
    ("text U+FFFF numeric reference", "&#xFFFF;");
    ("text literal U+FFFE", "\239\191\190&amp;");
    ("text literal U+FFFF with unknown entity", "\239\191\191&arp;");
    ("text literal U+FFFF leaves whole chunk raw", "\239\191\191&a&amp;");
    ("attribute surrogate numeric reference", "<b c=\"&#xd800;\">x</b>");
    ("attribute literal U+FFFF", "<b c=\"\239\191\191&amp;\">x</b>");
  ]
  |> List.map (fun (name, html) -> agrees name html)

let entity_guards =
  [
    ("unknown entity", "&arp;");
    ("zero numeric reference", "x&#0;y");
    ("out-of-range numeric reference", "x&#x110000;y");
    ("ordinary named entity", "a&amp;b");
    ("other BMP noncharacter", "x&#xFDD0;y");
    ("other supplementary noncharacter", "x&#x1FFFE;y");
    ("literal replacement character", "\239\191\189&amp;");
  ]
  |> List.map (fun (name, html) -> agrees name html)

let tree_builder_failures =
  [
    adapted_agrees "foreign buffered text run" "<math><b><svg>eP";
    agrees "Lite require_current_element"
      "<template><td><svg></td><tbody/><title></title><></U>";
    lite_parses "empty stack after formatting element"
      "<table><script></script><><s></script><td><svg></td><tr/><s/>";
    agrees "Lite above_in_stack"
      "<table><td><b><table><td><svg></td><script></script><><tr/><tr><td><svg></td></tr>M<P></b>";
  ]

let candidate_recovery_failures =
  [
    ("dropped xmp empty candidate", "<math><mo><Xmp></");
    ("dropped xmp named candidate", "<math><mo><xmp></x");
    ("dropped xmp mid-stream candidate", "<math><mo><xmp></b>c");
  ]
  |> List.map (fun (name, html) -> agrees name html)

let candidate_recovery_guards =
  [
    ("emitted rawtext element in foreign", "<math><mo><style></x");
    ("emitted rawtext element in html", "<div><style></x");
    ("emitted rcdata element in foreign", "<math><mo><title></x");
  ]
  |> List.map (fun (name, html) -> agrees name html)

let doctype_lookahead_failures =
  [
    ("keyword mismatch tail lowercased", "<!doctype a b>XYZw");
    ("fuzzer case", "<!doctype hte html>L~tmlml><stml><he");
    ("multibyte window", "<!doctype a X\xc3\xa9>ABCD");
    ("entity started in window", "<!doctype a b>&LT;a");
  ]
  |> List.map (fun (name, html) -> agrees name html)

let doctype_lookahead_guards =
  [
    ("public keyword", "<!doctype a public 'x'>YZ");
    ("gt outside window", "<!doctype a bcdefgh>XY");
    ("eof inside window", "<!doctype a b>X");
    ("lowercase tail", "<!doctype a b>xyz<B>T");
  ]
  |> List.map (fun (name, html) -> agrees name html)

let pre_newline_pushback =
  [
    (* [<a;>] produces no token of its own; it only splits the character run
       into [String "a\n"; String "\n"]. Lite used to consume the second token
       inside the [pre] handler and drop it, keeping one newline the oracle
       dropped. Needs pre/listing + an open formatting element + foreign
       content: the formatting element keeps the subtree buffer on, so
       [current_mode] stays pinned to the [pre] continuation and every character
       of foreign text re-enters it. *)
    adapted_agrees_with_errors
      "split foreign text run under pre and a formatting element"
      "<pre><a><svg>a\n<a;>\n";
    (* Error-stream only. The empty remainder has to be re-dispatched so that
       "in table" reports a second [bad content in 'table']. *)
    adapted_agrees_with_errors
      "pre as a direct table child with a leading newline" "<table><pre>\n";
  ]

let () =
  run_test_tt_main
    ("Lite fuzz regressions"
    >::: [
           "rawtext garbage recovery" >::: rawtext_failures;
           "rawtext guards" >::: rawtext_guards;
           "entity chunk fallback" >::: entity_failures;
           "entity guards" >::: entity_guards;
           "tree-builder invariants" >::: tree_builder_failures;
           "doctype keyword lookahead" >::: doctype_lookahead_failures;
           "doctype lookahead guards" >::: doctype_lookahead_guards;
           "end-tag candidate recovery" >::: candidate_recovery_failures;
           "end-tag candidate guards" >::: candidate_recovery_guards;
           "pre newline push-back" >::: pre_newline_pushback;
         ])
