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

(** Process [vat_queue] until it's empty, then return. The caller should then
    look for external sources of new events. *)
let run_vat () =
  while not (Queue.is_empty vat_queue) do
    (Queue.pop vat_queue) ()
  done

let promise_pair () : ('a promise * 'a resolver) =
  let p = ref (`Eventual (Queue.create ())) in

  let resolver (new_state:'a resolution) =
    match !p with
    | `Fulfilled _ | `Broken _ -> failwith "Promise already resolved!"
    | `Eventual callbacks ->
        p := (new_state :> 'a state);
        do_soon (fun () -> Queue.iter (fun fn -> fn new_state) callbacks)
  in
  (p, resolver)

let fulfill resolver x =
  resolver (`Fulfilled x)

let smash resolver x =
  resolver (`Broken x)

(** Queue up a callback to be invoked when [promise] is resolved (successfull or not).
    If [promise] is already resolved, queue it to be invoked next time we're idle.
    Return a new promise for the result of [cb]. *)
let when_resolved promise cb =
  let (new_promise, resolver) = promise_pair () in

  let () =
    match !promise with
    | `Fulfilled _ | `Broken _ as v ->
        do_soon (fun () ->
          try fulfill resolver (cb v)
          with ex -> smash resolver ex
        )
    | `Eventual q ->
        let wrapper v =
          try fulfill resolver (cb v)
          with ex -> smash resolver ex in
        Queue.add wrapper q in

  new_promise

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
