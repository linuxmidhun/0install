(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Support for asynchronous calls.
    This is a simpler (local-only) version of the scheme used by http://erights.org/ *)

(** A promise represents a value that was requested asynchronously. *)
type 'a promise

(** When the result of a promise is known, it becomes "resolved". A resolved promise can be either
    fulfilled (the expected result was produced) or broken (an error prevented fulfillment). *)
type 'a resolution =
  [ `Fulfilled of 'a
  | `Broken of exn]

(** Queue a callback to be run as soon as we are idle (currently no multi-threading support). *)
val do_soon : (unit -> unit) -> unit

(** Process all queued callbacks, then return. The caller should then look for
    external sources of new events. Note: this means that currently a process that
    keeps enqueing new idle jobs can block processing of external events. *)
val run_vat : unit -> unit

(** A resolver is used by the maker of a promise to provide its resolution. It can only be used once. *)
type 'a resolver = ('a resolution) -> unit

(** Convenience function to resolve a promise to a fulfillment (resolve it to Fulfilled). *)
val fulfill : 'a resolver -> 'a -> unit

(** Convenience function to break a promise (resolve it to Broken) *)
val smash : 'a resolver -> exn -> unit

(** Create a promise and a resolver for it. Pass the returned promise to the object to whom you are making
    the promise and keep the resolver for yourself. *)
val promise_pair : unit -> ('a promise * 'a resolver)

(** Create a promise that is already fulfilled. This is useful if you want to call an API which expects
    a promise, but you already know the value. *)
val make_fulfilled : 'a -> 'a promise

(** Queue up a callback to be invoked when [promise] is resolved (successfull or not).
    If [promise] is already resolved, queue [cb] to be invoked next time we're idle (with [do_soon]).
    @return a new promise for the result of [cb]. *)
val when_resolved : 'a promise -> ('a resolution -> 'b) -> 'b promise

(** Convenience wrapper for [when_resolved] that calls the callback only on success, passing the
    fulfillment. If the original promise is broken, the new promise for the result of the callback is
    also broken, with the same exception. *)
val when_fulfilled : 'a promise -> ('a -> 'b) -> 'b promise

(** If the promise becomes broken, call [callback ex]. Otherwise, do nothing. *)
val when_broken : unit promise -> (exn -> unit) -> unit

(** When the promise resolves, pass the resolution to the callback. If the callback throws an exception,
    it will escape out of the async system. *)
val when_resolved_ignored : 'a promise -> ('a resolution -> unit) -> unit

(** Get the successful value of a promise. If the promise is broken, re-raises its exception.
    It is an error to call this on a promise which is not yet resolved. *)
val get_fulfillment : 'a promise -> 'a

(** Returns the exception explaining why the promise was broken. Returns [None] if the promise is not broken. *)
val get_problem : 'a promise -> exn option

(** Run the top-level loop, processing queued events until the promise resolves. *)
val toplevel_wait_for : 'a promise -> 'a

(** Run the next queued callback. Returns false if the queue is empty. *)
val run_one_todo : unit -> bool

(** Check whether a promise has been resolved (i.e. is Fulfilled or Broken). *)
val is_resolved : 'a promise -> bool
