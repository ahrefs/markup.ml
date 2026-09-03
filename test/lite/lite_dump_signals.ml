let collect iter stream =
  let values = ref [] in
  iter (fun value -> values := value :: !values) stream;
  List.rev !values

type outcome = Signals of Markup_common.signal list | Raised of string

let run parse collect_signals input =
  try Signals (collect_signals (parse input))
  with exn -> Raised (Printexc.to_string exn)

let context_of_input input : [ `Document | `Fragment of string ] * string =
  let prefix = "FRAGMENT " in
  let plen = String.length prefix in
  if String.length input >= plen && String.sub input 0 plen = prefix then
    match String.index_from_opt input plen '\n' with
    | Some nl ->
        ( `Fragment (String.sub input plen (nl - plen)),
          String.sub input (nl + 1) (String.length input - nl - 1) )
    | None -> (`Document, input)
  else (`Document, input)

let oracle context input =
  run
    (fun input -> Oracle.parse ~context (fun _ _ -> ()) input)
    (collect Markup.iter) input

let lite context input =
  run
    (fun input -> Markup_lite.parse_html ~context input)
    (collect Markup_lite.iter) input

let print label = function
  | Raised exn -> Printf.printf "%s: RAISED %S\n" label exn
  | Signals signals ->
      Printf.printf "%s: %d signals\n" label (List.length signals);
      List.iteri
        (fun i signal ->
          Printf.printf "  %d: %S\n" i (Markup_common.signal_to_string signal))
        signals

let read_all channel =
  let buffer = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buffer channel 1
     done
   with End_of_file -> ());
  Buffer.contents buffer

let () =
  let input = read_all stdin in
  let context, input = context_of_input input in
  Printf.printf "input: %S\n%!" input;
  Printf.eprintf "oracle...\n%!";
  print "oracle" (oracle context input);
  Printf.eprintf "lite...\n%!";
  print "lite" (lite context input);
  Printf.eprintf "done\n%!"
