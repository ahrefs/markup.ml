(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

let html = "<p>&sup2;</p>"

let main_signals () =
  html
  |> Markup.string
  |> Markup.parse_html ~context:`Document
  |> Markup.signals
  |> Markup.to_list

let lite_signals () =
  let signals = ref [] in
  Markup_lite.parse_html html
  |> Markup_lite.iter (fun signal -> signals := signal :: !signals);
  List.rev !signals

let print_signals label signals =
  Printf.eprintf "%s:\n" label;
  List.iter
    (fun signal -> Printf.eprintf "  %s\n" (Markup.signal_to_string signal))
    signals

let () =
  let main = main_signals () in
  let lite = lite_signals () in
  if main <> lite then begin
    Printf.eprintf "Main and Lite differ for %S\n" html;
    print_signals "Main" main;
    print_signals "Lite" lite;
    exit 1
  end
