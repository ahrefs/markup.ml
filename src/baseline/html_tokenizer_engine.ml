open Common
module Core = Html_tokenizer_core

type state = Core.state
type token = Core.token
type input = Scalar of location * int | End

type action =
  | Await_input
  | Emit of location * token
  | Report of location * Error.t
  | Finished
  | Failed of exn

type input_waiter = { ended : unit -> unit; emit : location * int -> unit }
type report_waiter = { throw : exn -> unit; resume : unit -> unit }

type t = {
  stream : (location * token) Kstream.t;
  change_state : state -> unit;
  change_foreign : (unit -> bool) -> unit;
  mutable action : action option;
  mutable input_waiter : input_waiter option;
  mutable report_waiter : report_waiter option;
}

let create get_location =
  let owner = ref None in
  let get () =
    match !owner with
    | Some engine -> engine
    | None -> failwith "tokenizer engine used during initialization"
  in
  let input =
    Kstream.make (fun _throw ended emit ->
        let engine = get () in
        engine.input_waiter <- Some { ended; emit };
        engine.action <- Some Await_input)
  in
  let report location error throw resume =
    let engine = get () in
    engine.report_waiter <- Some { throw; resume };
    engine.action <- Some (Report (location, error))
  in
  let stream, change_state, change_foreign =
    Core.tokenize report (input, get_location)
  in
  let engine =
    {
      stream;
      change_state;
      change_foreign;
      action = None;
      input_waiter = None;
      report_waiter = None;
    }
  in
  owner := Some engine;
  engine

let rec drive engine =
  match engine.action with
  | Some (Emit _ as action)
  | Some (Finished as action)
  | Some (Failed _ as action) ->
      engine.action <- None;
      action
  | Some action -> action
  | None ->
      Kstream.next engine.stream
        (fun exn -> engine.action <- Some (Failed exn))
        (fun () -> engine.action <- Some Finished)
        (fun (location, token) ->
          engine.action <- Some (Emit (location, token)));
      drive engine

let supply engine input =
  match (engine.action, engine.input_waiter) with
  | Some Await_input, Some waiter ->
      engine.action <- None;
      engine.input_waiter <- None;
      begin match input with
      | Scalar (location, scalar) -> waiter.emit (location, scalar)
      | End -> waiter.ended ()
      end
  | _ -> invalid_arg "Html_tokenizer_engine.supply"

let take_report_waiter engine =
  match (engine.action, engine.report_waiter) with
  | Some (Report _), Some waiter ->
      engine.action <- None;
      engine.report_waiter <- None;
      waiter
  | _ -> invalid_arg "Html_tokenizer_engine report continuation"

let resume_report engine = (take_report_waiter engine).resume ()
let fail_report engine exn = (take_report_waiter engine).throw exn
let set_state engine state = engine.change_state state
let set_foreign engine foreign = engine.change_foreign foreign
