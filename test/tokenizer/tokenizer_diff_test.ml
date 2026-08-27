open OUnit2
open Tokenizer_diff

let located text =
  let rec loop column accumulator = function
    | [] -> (List.rev accumulator, (1, column))
    | character :: rest ->
        loop (column + 1)
          (((1, column), Char.code character) :: accumulator)
          rest
  in
  loop 1 [] (List.of_seq (String.to_seq text))

let check ?commands text =
  let scalars, eof_location = located text in
  let reference, candidate =
    match commands with
    | None -> compare_data ~eof_location scalars
    | Some commands -> script ~eof_location scalars commands
  in
  assert_equal reference candidate

let pulls count = List.init count (fun _ -> Next)

let tests =
  "tokenizer differential"
  >::: [
         ("data and references" >:: fun _ -> check "text\t&amp;&#x41;&#0;");
         ( "tags and attributes" >:: fun _ ->
           check "<p a='1' a=2 disabled><br/></p x>" );
         ( "comments declarations doctypes" >:: fun _ ->
           check "<!DOCTYPE html PUBLIC 'x'><!----><!--x--><!wat>" );
         ( "rcdata" >:: fun _ ->
           check "a&amp;</title>x" ~commands:(Set_state `RCDATA :: pulls 32) );
         ( "rawtext" >:: fun _ ->
           check "a</style>x" ~commands:(Set_state `RAWTEXT :: pulls 16) );
         ( "script" >:: fun _ ->
           check "<!--<script>--></script>x"
             ~commands:(Set_state `Script_data :: pulls 40) );
         ( "plaintext" >:: fun _ ->
           check "<&text" ~commands:(Set_state `PLAINTEXT :: pulls 16) );
         ( "foreign cdata" >:: fun _ ->
           check "<![CDATA[x<y]]>z" ~commands:(Set_foreign true :: pulls 24) );
         ( "state changes between pulls" >:: fun _ ->
           check "a<b&c"
             ~commands:
               [
                 Next;
                 Set_state `PLAINTEXT;
                 Next;
                 Set_state `Data;
                 Next;
                 Next;
                 Next;
                 Next;
                 Next;
               ] );
         ( "parser feedback" >:: fun _ ->
           let scalars, eof_location =
             located
               "<title>a&amp;</title><script><!--x--></script><svg><![CDATA[y]]></svg>"
           in
           let termination, _reports =
             shadow_parse (Some `Document) ~eof_location scalars
           in
           assert_equal `EOF termination );
       ]

let () = run_test_tt_main tests
