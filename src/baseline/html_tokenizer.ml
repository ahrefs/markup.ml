open Common
module Engine = Html_tokenizer_engine

type state = Engine.state
type token = Engine.token

let tokenize report (input, get_location) =
  let engine = Engine.create get_location in
  let rec pump throw ended emit =
    match Engine.drive engine with
    | Engine.Await_input ->
        Kstream.next input throw
          (fun () ->
            Engine.supply engine End;
            pump throw ended emit)
          (fun (location, scalar) ->
            Engine.supply engine (Scalar (location, scalar));
            pump throw ended emit)
    | Emit (location, token) -> emit (location, token)
    | Report (location, error) ->
        report location error
          (fun exn ->
            Engine.fail_report engine exn;
            pump throw ended emit)
          (fun () ->
            Engine.resume_report engine;
            pump throw ended emit)
    | Finished -> ended ()
    | Failed exn -> throw exn
  in
  let stream = Kstream.make pump in
  (stream, Engine.set_state engine, Engine.set_foreign engine)

type location_out = { mutable line : int; mutable column : int }

type pull = {
  engine : Engine.t;
  input : (location * int) Kstream.t;
  report : Error.parse_handler;
}

let create_pull report (input, get_location) =
  { engine = Engine.create get_location; input; report }

let next pull (out : location_out) =
  let rec loop () =
    match Engine.drive pull.engine with
    | Engine.Await_input ->
        let result = ref None in
        Kstream.next pull.input
          (fun exn -> result := Some (`Exception exn))
          (fun () -> result := Some `End)
          (fun scalar -> result := Some (`Scalar scalar));
        begin match !result with
        | Some (`Scalar (location, scalar)) ->
            Engine.supply pull.engine (Scalar (location, scalar))
        | Some `End -> Engine.supply pull.engine End
        | Some (`Exception exn) -> raise exn
        | None -> failwith "tokenizer input did not resume synchronously"
        end;
        loop ()
    | Emit ((line, column), token) ->
        out.line <- line;
        out.column <- column;
        token
    | Report (location, error) ->
        let resumed = ref false in
        pull.report location error
          (fun exn ->
            Engine.fail_report pull.engine exn;
            raise exn)
          (fun () ->
            resumed := true;
            Engine.resume_report pull.engine);
        if not !resumed then
          failwith "tokenizer report handler did not resume synchronously";
        loop ()
    | Finished -> failwith "tokenizer ended without an EOF token"
    | Failed exn -> raise exn
  in
  loop ()

let set_state pull state = Engine.set_state pull.engine state
let set_foreign pull foreign = Engine.set_foreign pull.engine foreign
