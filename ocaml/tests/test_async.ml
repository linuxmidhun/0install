(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

open OUnit

open Zeroinstall.Async

let suite = "async">::: [
  "fulfillment">:: (fun () ->
    let (promise, resolver) = promise_pair () in

    let value = ref 0 in
    let new_promise = when_fulfilled promise (fun v -> value := v) in
    assert (!value = 0);

    fulfill resolver 7;
    assert (!value = 0);      (* Happens later *)

    run_vat ();
    assert (!value = 7);

    assert (get_fulfillment new_promise = ());
  );

  "problem">:: (fun () ->
    let (promise, resolver) = promise_pair () in

    let value = ref 0 in
    let new_promise = when_fulfilled promise (fun v -> value := v) in
    assert (!value = 0);

    let ex = Failure "failed" in
    smash resolver ex;
    assert (get_problem promise = Some ex);
    assert (get_problem new_promise = None);    (* Happens later *)
    run_vat ();
    assert (get_problem new_promise = Some ex);
    assert (!value = 0);      

    try ignore (get_fulfillment new_promise); assert false
    with Failure msg ->
      Fake_system.assert_str_equal "failed" msg
  );

  "chained_exceptions">:: (fun () ->
    let (promise, resolver) = promise_pair () in

    let value = ref 0 in
    let new_promise1 = when_fulfilled promise (fun _ -> failwith "oops") in
    let new_promise2 = when_fulfilled promise (fun v -> value := v) in

    fulfill resolver 7;
    run_vat ();
    assert (get_problem promise = None);

    assert (get_problem new_promise1 = Some (Failure "oops"));
    assert (get_problem new_promise2 = None);
    assert (!value = 7);
  );
]
