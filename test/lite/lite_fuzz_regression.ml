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
    agrees "Lite require_current_element"
      "<template><td><svg></td><tbody/><title></title><></U>";
    agrees "baseline require_current_element"
      "<table><script></script><><s></script><td><svg></td><tr/><s/>";
    agrees "Lite above_in_stack"
      "<table><td><b><table><td><svg></td><script></script><><tr/><tr><td><svg></td></tr>M<P></b>";
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
         ])
