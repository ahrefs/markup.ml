(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

let measure runs library source format f =
  let name = Printf.sprintf "%s: %s (%s)" library source format in

  let rec run = function
    | 0 -> ()
    | n -> f (); run (n - 1)
  in

  let start_time = Unix.gettimeofday () in

  run runs;

  let duration = (Unix.gettimeofday ()) -. start_time in
  let average = duration /. (float_of_int runs) *. 1000000. in

  Printf.printf "  %s: %.0f us\n" name average

let all_pages = [`Html, "test/pages/google"; `Xml, "test/pages/xml_spec"; `Html, "test/pages/problem_oom_01"]

let do_full_benchmark ?(runs = 100) ?parse_html ?parse_xml library =
  List.iter 
    (function
      | (`Html, page) -> Option.iter (fun f -> measure runs library page "html" (fun () -> f page)) parse_html
      | (`Xml, page) ->  Option.iter (fun f -> measure runs library page "xml" (fun () -> f page)) parse_xml)
    all_pages
