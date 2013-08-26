(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Interfacing with the old Python code *)

open General
open Support.Common
module Q = Support.Qdom
module A = Async

let finally = Support.Utils.finally

let slave_debug_level = ref None      (* Inherit our logging level *)

let get_command config args : string list =
  let result = ref [] in
  let try_with path =
    if config.system#file_exists path then (
      (* Note: on Windows, we need to specify "python" *)
      result := "python" :: path :: "--python-fallback" :: args;
      true
    ) else (
      false
    ) in
  let my_dir = Filename.dirname config.abspath_0install in
  let parent_dir = Filename.dirname my_dir in
  ignore (
    try_with (my_dir +/ "0launch") ||                        (* When installed in /usr/bin *)
    try_with (parent_dir +/ "0launch") ||                    (* When running from ocaml directory *)
    try_with (Filename.dirname parent_dir +/ "0launch") ||   (* When running from _build directory *)
    failwith "Can't find 0launch command!"
  );
  assert (!result <> []);
  !result

(** Run "python -m zeroinstall.cmd". If ../zeroinstall exists, put it in PYTHONPATH,
    otherwise use the system version of 0install. *)
let fallback_to_python config args =
  config.system#exec ~search_path:true (get_command config args)

let bool_opt name = function
  | false -> []
  | true -> [name]

let rec store_opts = function
  | [] -> []
  | x::xs -> "--with-store" :: x :: store_opts xs

type child_connection = {
  child_pid : int;
  to_child : out_channel;
  from_child : in_channel;
}

(** Runs a Python slave process. Remembed to close the connection when done. *)
open Yojson.Basic
class slave config =
  (* let () = log_warning "CREATE SLAVE" in *)

  let system = config.system in

  let connection = ref None in
  
  let get_connection () =
    (* log_warning "START SLAVE"; *)
    match !connection with
    | Some c -> c
    | None ->
        let (child_stdin_r, child_stdin_w) = Unix.pipe () in
        let (child_stdout_r, child_stdout_w) = Unix.pipe () in

        let debug_args =
          let open Support.Logging in
          let t =
            match !slave_debug_level with
            | None -> !threshold
            | Some t -> t in
          match t with
          | Debug -> ["-vv"]
          | Info -> ["-v"]
          | Warning -> [] in

        let extra_args = List.concat [
          debug_args;
          bool_opt "--dry-run" config.dry_run;
          store_opts config.extra_stores;
          bool_opt "--offline" (config.network_use = Offline);
        ] in

        let argv = get_command config ("slave" :: extra_args) in
        let child_pid =
          try
            Unix.set_close_on_exec child_stdin_w;
            Unix.set_close_on_exec child_stdout_r;
            finally (fun () -> Unix.close child_stdin_r; Unix.close child_stdout_w) ()
                    (fun () -> system#create_process argv child_stdin_r child_stdout_w Unix.stderr)
          with ex ->
            Unix.close child_stdin_w;
            Unix.close child_stdout_r;
            raise ex in
        
        let to_child = Unix.out_channel_of_descr child_stdin_w in
        let from_child = Unix.in_channel_of_descr child_stdout_r in

        let c = {child_pid; to_child; from_child} in
        connection := Some c;
        c in

  let send_json c ?xml request =
      let data = to_string request in
      log_info "Sending to Python: %s" data;
      Printf.fprintf c.to_child "%d\n" (String.length data);
      output_string c.to_child data;

      let () =
        match xml with
        | Some xml ->
            let data = Q.to_utf8 xml in
            log_info "... with XML: %s" data;
            Printf.fprintf c.to_child "%d\n" (String.length data);
            output_string c.to_child data;
        | None -> () in

      flush c.to_child in

  let async_tickets = Hashtbl.create 10 in
  let next_ticket = ref Int64.zero in

  let read_response c fn =
    let l =
      let line = input_line c.from_child in
      try int_of_string line
      with Failure _ -> raise_safe "Invalid response from slave '%s' (expected integer). This is a bug." (String.escaped line) in
    let buf = String.create l in
    really_input c.from_child buf 0 l;
    log_info "Response from Python: %s" buf;
    try fn @@ from_string buf
    with Safe_exception _ as ex ->
      reraise_with_context ex "... processing JSON response from Python slave:\n%s" buf in

  let process_resolved ticket response =
    let cb =
      try
        let ticket = Int64.of_string ticket in
        let cb = Hashtbl.find async_tickets ticket in
        Hashtbl.remove async_tickets ticket;
        cb
      with _ -> raise_safe "Unknown async ticket '%s'" ticket in
    cb response in

  let process_async_response response =
    match response with
    | `List [`String "resolved"; `String ticket; response] -> process_resolved ticket response
    | `List [`String "input"; `String prompt] ->
        (* Ask on stderr, because we may be writing XML to stdout *)
        prerr_string prompt; flush stdout;
        let user_input = input_line stdin in
        let c = get_connection () in
        send_json c (`String user_input)
    | _ -> raise_safe "Unexpected JSON response" in

  object (self)
    (** Send a JSON message to the Python slave and return whatever data it sends back. *)
    method invoke : 'a. json -> ?xml:Q.element -> (json -> 'a) -> 'a = fun request ?xml parse_fn ->
      let c = get_connection () in

      send_json c ?xml request;

      (* Normally we just get a single reply, but we might have to handle some input requests first. *)
      let rec loop () =
        read_response c (function
          | `List [`String "error"; `String err] -> raise_safe "%s" err
          | `List [`String "ok"; r] -> parse_fn r
          | response -> process_async_response response; loop ()
        )
      in loop ()

    method invoke_async : 'a. json -> ?xml:Q.element -> (json -> 'a) -> 'a A.promise =
      fun request ?xml parse_fn ->
        let (promise, resolver) = A.promise_pair() in
        let ticket = !next_ticket in 
        next_ticket := Int64.add !next_ticket Int64.one;
        let callback json =
          try
            match json with
            | `List [`String "ok"; r] -> A.fulfill resolver (parse_fn r)
            | `List [`String "error"; `String msg] -> A.smash resolver (Safe_exception (msg, ref []))
            | _ -> raise_safe "Invalid async response from Python"
          with ex -> A.smash resolver ex in
        Hashtbl.add async_tickets ticket callback;
        self#invoke (`List [`String "invoke-async"; `String (Int64.to_string ticket); request]) ?xml ignore;
        promise

    method close =
      (* log_warning "CLOSE SLAVE"; *)
      match !connection with
      | None -> ()
      | Some c ->
          log_info "Closing connection to slave";
          close_out c.to_child;
          close_in c.from_child;
          system#reap_child c.child_pid;
          connection := None;
          log_info "Slave terminated"

    (** Process the queue until promise resolves. If the queue is empty, read responses from the GUI. *)
    method run_async : 'a. 'a A.promise -> unit = fun promise ->
      while not @@ A.is_resolved promise do
        if not @@ A.run_one_todo () then (
          (* Vat queue is empty; check for messages from slave *)
          if Hashtbl.length async_tickets = 0 then
            failwith "Can't resolve promise: local queue is empty and no outstanding requests with slave"
          else
            read_response (get_connection ()) process_async_response    (* Will add something to the queue *)
        )
      done

    method system = system
  end
