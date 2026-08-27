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

let print_signals signals =
  signals
  |> List.map Markup_common.signal_to_string
  |> String.concat "\n  " |> Printf.sprintf "\n  %s"

let agrees ?(context = `Document) name html =
  name >:: fun _ ->
  assert_equal ~printer:print_signals (baseline context html)
    (lite context html)

let disagrees ?(context = `Document) name html =
  name >:: fun _ ->
  assert_bool "baseline and lite now agree; promote this test to [agrees]"
    (baseline context html <> lite context html)

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
           "crlf"
           >::: [
                  agrees "crlf in text" "<p>a\r\nb</p>"; agrees "lone cr" "a\rb";
                ];
           "bom"
           >::: [
                  agrees "leading bom" "\xEF\xBB\xBFa";
                  agrees "bom in text" "a\xEF\xBB\xBFb";
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
           "known divergence: foreign breakout reentry"
           >::: [
                  disagrees "svg b svg text" "<svg><b><svg>ab";
                  disagrees "math b math text" "<math><b><math>xy";
                  disagrees "svg s svg digits" "<svg><s><svg>00";
                ];
           "known divergence: cdata in foreign content"
           >::: [
                  disagrees "svg" "<svg><![CDATA[a]]></svg>";
                  disagrees "math" "<math><![CDATA[1]]></math>";
                ];
           "known divergence: fragment breakout rawtext eof"
           >::: [
                  disagrees ~context:(`Fragment "svg") "style slash"
                    "<p></p><style></";
                  disagrees ~context:(`Fragment "svg") "script candidate"
                    "<p></p><script></x";
                  disagrees ~context:(`Fragment "math") "style candidate"
                    "<p></p><style></x";
                ];
         ])
