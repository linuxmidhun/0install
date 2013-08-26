(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Support for asynchronous calls.
    This is a simpler (local-only) version of the scheme used by http://erights.org/ *)

type 'a resolution =
  [ `Fulfilled of 'a
  | `Broken of exn]

type 'a state =
  [ 'a resolution
  | `Eventual of ('a resolution -> unit) Queue.t ]

type 'a promise = 'a state ref

type 'a resolver = ('a resolution) -> unit

let vat_queue = Queue.create ()

let do_soon cb =
  Queue.add cb vat_queue

let run_one_todo () =
  if Queue.is_empty vat_queue then false
  else (Queue.pop vat_queue (); true)

(** Process [vat_queue] until it's empty, then return. The caller should then
    look for external sources of new events. *)
let run_vat () =
  while run_one_todo () do () done

let promise_pair () : ('a promise * 'a resolver) =
  let p = ref (`Eventual (Queue.create ())) in

  let resolver (new_state:'a resolution) =
    match !p with
    | `Fulfilled _ | `Broken _ -> failwith "Promise already resolved!"
    | `Eventual callbacks ->
        p := (new_state :> 'a state);
        Queue.iter (fun fn -> do_soon (fun () -> fn new_state)) callbacks
  in
  (p, resolver)

let make_fulfilled value = ref (`Fulfilled value)

let fulfill resolver x =
  resolver (`Fulfilled x)

let smash resolver x =
  resolver (`Broken x)

let when_resolved_ignored promise cb =
  match !promise with
  | `Fulfilled _ | `Broken _ as v -> do_soon (fun () -> cb v)
  | `Eventual q -> Queue.add cb q

(** Queue up a callback to be invoked when [promise] is resolved (successfull or not).
    If [promise] is already resolved, queue it to be invoked next time we're idle.
    Return a new promise for the result of [cb]. *)
let when_resolved promise cb =
  let (new_promise, resolver) = promise_pair () in

  when_resolved_ignored promise (fun v ->
    try fulfill resolver (cb v)
    with ex -> smash resolver ex
  );

  new_promise

let when_broken promise cb =
  when_resolved_ignored promise (function
    | `Broken ex -> cb ex
    | `Fulfilled () -> ()
  )

let when_fulfilled promise cb =
  when_resolved promise (function
    | `Fulfilled x -> cb x
    | `Broken ex -> raise ex
  )

let get_fulfillment p =
  match !p with
  | `Fulfilled x -> x
  | `Broken ex -> raise ex
  | `Eventual _ -> failwith "Promise not fulfilled yet!"

let get_problem p =
  match !p with
  | `Broken ex -> Some ex
  | `Fulfilled _ | `Eventual _ -> None

let is_resolved promise =
  match !promise with
  | `Fulfilled _ | `Broken _ -> true
  | `Eventual _ -> false

let toplevel_wait_for promise =
  while not (is_resolved promise) do
    Queue.pop vat_queue ()
  done;
  get_fulfillment promise
