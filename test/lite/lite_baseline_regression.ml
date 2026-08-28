open OUnit2

let collect iter stream =
  let signals = ref [] in
  iter (fun signal -> signals := signal :: !signals) stream;
  List.rev !signals

let baseline context html =
  Markup.string html |> Markup.parse_html ~context |> Markup.signals
  |> Markup.to_list

let lite context html =
  collect Markup_lite.iter
    (Markup_lite.parse_html ~report:(fun _ _ -> ()) ~context html)

let lite_tokens ?(report = fun _ _ -> ()) context tokens =
  collect Markup_lite.iter (Markup_lite.parse_tokens ~report ~context tokens)

let token_tag name =
  Markup_lite.Token_tag.{ name; attributes = []; self_closing = false }

let print_signals signals =
  signals
  |> List.map Markup_common.signal_to_string
  |> String.concat "\n  " |> Printf.sprintf "\n  %s"

let agrees ?(context = `Document) name html =
  name >:: fun _ ->
  assert_equal ~printer:print_signals (baseline context html)
    (lite context html)

let agrees_with_text ?(context = `Document) name html text =
  name >:: fun _ ->
  let expected = baseline context html in
  let actual = lite context html in
  assert_equal ~printer:print_signals expected actual;
  assert_bool "expected text signal was not preserved"
    (List.exists
       (function
         | `Text strings -> String.concat "" strings = text | _ -> false)
       actual)

let terminates =
  "in row misnested math" >:: fun _ ->
  ignore (lite `Document "<table><tr><math></tr><td><tr><b><math>0")

exception Parser_timeout

let baseline_loops =
  "in row misnested svg" >:: fun _ ->
  let html = "<table><tr><math></tr><td><tr><b><svg>d" in
  ignore (lite `Document html);
  let previous_handler =
    Sys.signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> raise_notrace Parser_timeout))
  in
  let previous_timer =
    Unix.setitimer Unix.ITIMER_REAL { it_interval = 0.; it_value = 0.25 }
  in
  let outcome =
    Fun.protect
      ~finally:(fun () ->
        ignore (Unix.setitimer Unix.ITIMER_REAL previous_timer);
        Sys.set_signal Sys.sigalrm previous_handler)
      (fun () ->
        try
          ignore (baseline `Document html);
          `Terminated
        with Parser_timeout -> `Timed_out)
  in
  assert_equal
    ~printer:(function
      | `Terminated -> "terminated" | `Timed_out -> "timed out")
    `Timed_out outcome

let () =
  run_test_tt_main
    ("Lite vs baseline regressions"
    >::: [
           "comments"
           >::: [
                  agrees "simple" "<!-- c --><p>x</p>";
                  agrees "empty" "a<!---->b";
                ];
           "doctype"
           >::: [
                  agrees "lowercase" "<!doctype html><p>x</p>";
                  agrees "uppercase" "<!DOCTYPE HTML>text";
                ];
           "br end tag"
           >::: [ agrees "between text" "x</br>y"; agrees "alone" "</br>" ];
           "self-closing"
           >::: [ agrees "div" "<div/>x"; agrees "span and b" "<span/>x<b/>y" ];
           "rcdata"
           >::: [
                  agrees "textarea start tag" "<textarea><b></textarea>";
                  agrees "textarea end tag" "<textarea>a</i>b</textarea>x";
                ];
           "script escaping"
           >::: [
                  agrees "escaped close tag"
                    "<script><!-- </script> --></script>";
                  agrees "double escaped"
                    "<script><!-- <script>x</script> --></script>y";
                ];
           "lenient entities"
           >::: [
                  agrees "named without semicolon" "a&ampb";
                  agrees "numeric without semicolon" "x&#38y";
                  agrees "attribute named without semicolon"
                    "<p title=\"a&ampb\">x</p>";
                ];
           "invalid utf8"
           >::: [
                  agrees "leading byte" "\xff<p>x</p>";
                  agrees "byte in text" "<p>a\xffb</p>";
                  agrees "continuation bytes" "\xb5\x95";
                ];
           "declared encoding"
           >::: [
                  agrees "windows-1251"
                    "<meta charset=windows-1251><p>\xCF\xF0\xE8\xE2\xE5\xF2";
                  agrees "baseline windows-1251 D0 mapping"
                    "<meta charset=windows-1251><p>\xD0";
                  agrees "HTML iso-8859-1 is windows-1252"
                    "<meta charset=iso-8859-1><p>caf\xE9";
                  agrees "UTF-8 BOM takes precedence"
                    "\xEF\xBB\xBF<meta charset=windows-1251><p>caf\xC3\xA9";
                ];
           "crlf"
           >::: [
                  agrees "crlf in text" "<p>a\r\nb</p>"; agrees "lone cr" "a\rb";
                ];
           "bom"
           >::: [
                  agrees "leading bom" "\xEF\xBB\xBFa";
                  agrees_with_text "internal bom is preserved" "a\xEF\xBB\xBFb"
                    "a\xEF\xBB\xBFb";
                  agrees "two leading boms" "\xEF\xBB\xBF\xEF\xBB\xBFa";
                ];
           "table whitespace"
           >::: [
                  agrees "before colgroup" "<table>\n\t<colgroup><col></table>";
                  agrees "between rows"
                    "<table><tr><td>a</td></tr> <tr></table>";
                ];
           "attributes"
           >::: [
                  agrees "source order" "<p a=1 b=2 c=3>x";
                  agrees "duplicates" "<p a=1 a=2 A=3>x";
                ];
           "garbage tags"
           >::: [
                  agrees "at sign in name" "<x@y>z";
                  agrees "with attribute and end tag"
                    "<foo@bar baz=1>t</foo@bar>";
                ];
           "tag open text"
           >::: [
                  agrees "empty tag" "<>x";
                  agrees "comparison operators" "text < 5 and > 3";
                  agrees "space before name" "< div>x</div>";
                ];
           "attribute names"
           >::: [
                  agrees "quote as name" "<div \"=\"\">x";
                  agrees "equals as name" "<p =\"v\">x";
                  agrees "control character in name" "<p a\x01=1>x";
                ];
           "rawtext elements"
           >::: [
                  agrees "noembed" "<noembed><span>x</noembed>y";
                  agrees "xmp" "<xmp>a<b>c</xmp>d";
                ];
           "plaintext"
           >::: [
                  agrees "swallows rest" "<p><plaintext>a<b>";
                  agrees "end tag is text" "<plaintext>x</plaintext>y";
                ];
           "fragment foreign"
           >::: [
                  agrees ~context:(`Fragment "svg") "td in svg" "<td>x";
                  agrees ~context:(`Fragment "svg") "div span in svg"
                    "<div><span></div>";
                ];
           "parse_tokens"
           >::: [
                  ( "document" >:: fun _ ->
                    let tokens =
                      [
                        ((1, 1), `Start (token_tag "p"));
                        ((1, 4), `String "x");
                        ((1, 5), `End (token_tag "p"));
                        ((1, 9), `EOF);
                      ]
                    in
                    assert_equal ~printer:print_signals
                      (lite `Document "<p>x</p>")
                      (lite_tokens `Document tokens) );
                  ( "fragment" >:: fun _ ->
                    let tokens = [ ((1, 1), `String "<b>"); ((1, 4), `EOF) ] in
                    assert_equal ~printer:print_signals
                      (lite (`Fragment "textarea") "&lt;b>")
                      (lite_tokens (`Fragment "textarea") tokens) );
                  ( "report location" >:: fun _ ->
                    let reports = ref [] in
                    let tokens =
                      [ ((7, 11), `End (token_tag "p")); ((7, 15), `EOF) ]
                    in
                    ignore
                      (lite_tokens
                         ~report:(fun location error ->
                           reports := (location, error) :: !reports)
                         `Document tokens);
                    assert_bool "token location was not reported"
                      (List.exists
                         (fun (location, _) -> location = (7, 11))
                         !reports) );
                ];
           "foreign breakout reentry"
           >::: [
                  agrees "svg b svg text" "<svg><b><svg>ab";
                  agrees "math b math text" "<math><b><math>xy";
                  agrees "svg s svg digits" "<svg><s><svg>00";
                ];
           "cdata in foreign content"
           >::: [
                  agrees "svg" "<svg><![CDATA[a]]></svg>";
                  agrees "math" "<math><![CDATA[1]]></math>";
                ];
           "form feed whitespace"
           >::: [
                  agrees "alone" "\x0C";
                  agrees "after col" "<table><col>\x0C";
                  agrees "after template" "<template></template>\x0C";
                ];
           "nul does not reconstruct formatting"
           >::: [
                  agrees "p b p" "<p><b><p>\x00";
                  agrees "li s li" "<li><s><li>\x00<p";
                ];
           "fragment breakout rawtext eof"
           >::: [
                  agrees ~context:(`Fragment "svg") "style slash"
                    "<p></p><style></";
                  agrees ~context:(`Fragment "svg") "script candidate"
                    "<p></p><script></x";
                  agrees ~context:(`Fragment "math") "style candidate"
                    "<p></p><style></x";
                  agrees "document candidate" "<style></x";
                  agrees ~context:(`Fragment "svg") "complete candidate"
                    "<p></p><style></x>";
                ];
           "termination" >::: [ terminates; baseline_loops ];
         ])
