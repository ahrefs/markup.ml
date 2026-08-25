open OUnit2

let tag name attributes : Markup__Common.Token_tag.t =
  {name; attributes; self_closing = false}

let tokens_without_locations html =
  html |> Ragel_html_tokenizer.tokenize |> List.map snd

let tests =
  [
    ( "private.html-tokenize.tokens" >:: fun _ ->
      let actual =
        tokens_without_locations
          "<br/><p a='&amp;'>&lt;</p><script>&amp;</script><style>x</style>"
      in
      let expected : Markup__Html_tokenizer.token list =
        [
          `Start (tag "br" []);
          `Start (tag "p" ["a", "&"]);
          `String "<";
          `End (tag "p" []);
          `Start (tag "script" []);
          `String "&amp;";
          `End (tag "script" []);
          `Start (tag "style" []);
          `String "x";
          `End (tag "style" []);
          `EOF;
        ]
      in
      assert_equal expected actual );
    ( "private.html-tokenize.locations" >:: fun _ ->
      let actual = Ragel_html_tokenizer.tokenize "one\ntwo" in
      assert_equal [((2, -1), `String "one\ntwo"); ((2, -1), `EOF)] actual );
  ]
