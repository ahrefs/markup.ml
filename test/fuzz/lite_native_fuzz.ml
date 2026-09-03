let maximum_input_length = 1024 * 1024

let read_input channel =
  let buffer = Buffer.create 4096 in
  let bytes = Bytes.create 65536 in
  let rec read total =
    let count = input channel bytes 0 (Bytes.length bytes) in
    if count = 0 then Some (Buffer.contents buffer)
    else
      let total = total + count in
      if total > maximum_input_length then None
      else begin
        Buffer.add_subbytes buffer bytes 0 count;
        read total
      end
  in
  read 0

(* "FRAGMENT <name>\n<body>" parses <body> in fragment context <name>;
   anything else parses the whole input as a document. *)
let context_of_input input : [ `Document | `Fragment of string ] * string =
  let prefix = "FRAGMENT " in
  let prefix_length = String.length prefix in
  if
    String.length input >= prefix_length
    && String.sub input 0 prefix_length = prefix
  then
    match String.index_from_opt input prefix_length '\n' with
    | Some newline ->
        ( `Fragment (String.sub input prefix_length (newline - prefix_length)),
          String.sub input (newline + 1) (String.length input - newline - 1) )
    | None ->
        ( `Fragment
            (String.sub input prefix_length
               (String.length input - prefix_length)),
          "" )
  else (`Document, input)

let crash exception_ =
  Printf.eprintf "markup.lite native parser raised: %s\n%!"
    (Printexc.to_string exception_);
  Unix.kill (Unix.getpid ()) Sys.sigabrt;
  exit 2

let check input =
  let context, body = context_of_input input in
  try
    Markup_lite.parse_html ~context ~report:(fun _ _ -> ()) body
    |> Markup_lite.iter (fun _ -> ())
  with exn -> crash exn

let () = match read_input stdin with Some input -> check input | None -> ()
