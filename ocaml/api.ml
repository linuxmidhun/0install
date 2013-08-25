(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Allows driving the solver over a socket. *)

open Zeroinstall.General
open Options
open Support.Common
module Qdom = Support.Qdom
module Feed_cache = Zeroinstall.Feed_cache
module Feed = Zeroinstall.Feed
module Requirements = Zeroinstall.Requirements

let debug = false

let read_json () =
  if debug then prerr_endline "Waiting for length...";
  let l = int_of_string @@ input_line stdin in
  if debug then prerr_endline "Waiting for JSON...";
  let buf = String.create l in
  really_input stdin buf 0 l;
  if debug then prerr_endline "Got JSON";
  Yojson.Basic.from_string buf

let send_status msg =
  print_endline msg

class python_feed_provider config distro =
  let feeds_used = ref StringSet.empty in

  object (_ : #Zeroinstall.Feed_cache.feed_provider)
    method get_feed url =
      feeds_used := StringSet.add url !feeds_used;

      print_endline "FEED";
      print_endline url;
      if debug then prerr_endline "Waiting for feed...";
      let l = int_of_string @@ input_line stdin in
      if debug then prerr_endline "Got length";
      let buf = String.create l in
      really_input stdin buf 0 l;
      if debug then prerr_endline "Got XML";
      let root =
        try
          Support.Qdom.parse_input (Some url) (Xmlm.make_input (`String (0, buf)))
        with ex ->
          log_warning ~ex "Bad XML from Python:\n%s" buf;
          raise ex in
      if ZI.tag root = Some "missing" then None
      else (
        let overrides = {Feed.last_checked = None; Feed.user_stability = StringMap.empty} in
        let local_path =
          if Support.Utils.path_is_absolute url then (
            Some url
          ) else None in
        Some (Feed.parse config.system root local_path, overrides)
      )

    method get_distro_impls feed =
      let url = "distribution:" ^ feed.Feed.url in
      match Zeroinstall.Distro.get_package_impls distro feed with
      | None -> None
      | Some impls ->
          feeds_used := StringSet.add url !feeds_used;
          let overrides = Feed.load_feed_overrides config url in
          Some (impls, overrides)

    method get_iface_config _uri =
      {Feed_cache.stability_policy = None; Feed_cache.extra_feeds = [];}

    method get_feeds_used () = StringSet.elements !feeds_used

    method have_stale_feeds () = false
  end

let handle options flags args =
  Support.Argparse.iter_options flags (function
    | #common_option as o -> Common_options.process_common_option options o
  );
  assert (args = []);

  let reqs = Requirements.parse_requirements (read_json ()) in
  let feed_provider = new python_feed_provider options.config (Lazy.force options.distro) in

  let () =
    match input_line stdin with
    | "SOLVE" -> (
        let (ready, results) = Zeroinstall.Solver.solve_for options.config feed_provider reqs in
        if ready then
          send_status "SUCCESS"
        else
          send_status "FAIL";

        let sels = results#get_selections in
        let buf = Buffer.create 1000 in
        let out = Xmlm.make_output @@ `Buffer buf in
        Qdom.output out sels;
        Buffer.add_char buf '\n';
        let data = Buffer.contents buf in
        Printf.printf "%d\n" (String.length data);
        output_string stdout data;

        let json_feeds = `List (List.map (fun f -> `String f) (feed_provider#get_feeds_used ())) in
        let data = Yojson.Basic.to_string json_feeds in
        Printf.printf "%d\n" (String.length data);
        output_string stdout data;

        if not ready then (
          let data = Zeroinstall.Diagnostics.get_failure_reason options.config results in
          Printf.printf "%d\n" (String.length data);
          output_string stdout data
        );
    )
    | "JUSTIFY" -> (
        let l = int_of_string (input_line stdin) in
        let buf = String.create l in
        really_input stdin buf 0 l;
        match Yojson.Basic.from_string buf with
        | `List [`String iface; `String feed; `String id] ->
            let wanted_id = Feed.({feed = feed; id = id}) in
            let data = Zeroinstall.Diagnostics.justify_decision options.config feed_provider reqs iface wanted_id in
            send_status "SUCCESS";
            Printf.printf "%d\n" (String.length data);
            output_string stdout data;
            flush stdout
        | _ -> failwith buf
    )
    | x -> failwith x
  in
  flush stdout
