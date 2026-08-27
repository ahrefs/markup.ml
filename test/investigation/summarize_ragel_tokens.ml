module Snapshot = Test_support.Ragel_token_snapshot

let input = stdin

let () =
  let magic = input_line input in
  if magic <> "MARKUP-RAGEL-TOKENS-1" then failwith "bad snapshot header";
  let cases = ref 0 in
  let corpus_cases = ref 0 in
  let regression_cases = ref 0 in
  let tokens = ref 0 in
  let starts = ref 0 in
  let ends = ref 0 in
  let strings = ref 0 in
  let chars = ref 0 in
  let comments = ref 0 in
  let doctypes = ref 0 in
  let eofs = ref 0 in
  let self_closing = ref 0 in
  let string_bytes = ref 0 in
  let nul_strings = ref 0 in
  let form_feed_strings = ref 0 in
  let nul_sources = ref [] in
  let form_feed_sources = ref [] in
  let max_tokens = ref ("", 0) in
  let rec read () =
    match (Marshal.from_channel input : Snapshot.t) with
    | snapshot ->
        incr cases;
        if String.starts_with ~prefix:"corpus:" snapshot.source then
          incr corpus_cases
        else incr regression_cases;
        let count = List.length snapshot.tokens in
        if count > snd !max_tokens then max_tokens := (snapshot.source, count);
        tokens := !tokens + count;
        List.iter
          (fun (_, token) ->
            match token with
            | `Start tag ->
                incr starts;
                if tag.Markup__Common.Token_tag.self_closing then
                  incr self_closing
            | `End _ -> incr ends
            | `String text ->
                incr strings;
                string_bytes := !string_bytes + String.length text;
                if String.contains text '\x00' then begin
                  incr nul_strings;
                  nul_sources := snapshot.source :: !nul_sources
                end;
                if String.contains text '\x0C' then begin
                  incr form_feed_strings;
                  form_feed_sources := snapshot.source :: !form_feed_sources
                end
            | `Char _ -> incr chars
            | `Comment _ -> incr comments
            | `Doctype _ -> incr doctypes
            | `EOF -> incr eofs)
          snapshot.tokens;
        read ()
    | exception End_of_file -> ()
  in
  read ();
  Printf.printf
    "cases=%d corpus_cases=%d regression_literal_cases=%d\n\
     tokens=%d start=%d end=%d string=%d char=%d comment=%d doctype=%d eof=%d\n\
     string_bytes=%d nul_strings=%d form_feed_strings=%d self_closing=%d\n\
     max_tokens=%d max_source=%S\n"
    !cases !corpus_cases !regression_cases !tokens !starts !ends !strings !chars
    !comments !doctypes !eofs !string_bytes !nul_strings !form_feed_strings
    !self_closing (snd !max_tokens) (fst !max_tokens);
  List.sort_uniq String.compare !nul_sources
  |> List.iter (Printf.printf "nul_source=%S\n");
  List.sort_uniq String.compare !form_feed_sources
  |> List.iter (Printf.printf "form_feed_source=%S\n")
