(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

open General
open Support.Common

module U = Support.Utils
module Q = Support.Qdom
module G = Support.Gpg

type try_mirror_case = [ `problem of string ]
type non_mirror_case = [ `ok of Q.element | `no_trusted_keys | `replay_attack of (feed_url * float * float) | `aborted_by_user ]

type fetch_feed_response =
  [ `update of (Q.element * fetch_feed_response Lwt.t option)  (* Use this version (but a better version may come soon) *)
  | `aborted_by_user        (* Abort silently (no need to notify the user) *)
  | `problem of (string * fetch_feed_response Lwt.t option)    (* Report a problem (but may still succeed later) *)
  | `no_update ]            (* Use the previous version *)

let get_response = function
  | `ok result -> `update (result, None)
  | `aborted_by_user -> `aborted_by_user
  | `no_trusted_keys ->         (* Don't bother trying the mirror if we have a trust problem *)
      `problem ("Not signed with a trusted key", None)
  | `replay_attack (url, old_time, new_time) ->
      let old_time = old_time |> Unix.localtime |> U.format_time_pretty in
      let new_time = new_time |> Unix.localtime |> U.format_time_pretty in
      (* Don't bother trying the mirror if we have a replay attack *)
      let msg = Printf.sprintf (
        "New feed's modification time is before old version!\n" ^^
        "Interface: %s\nOld time: %s\nNew time: %s\n" ^^
        "Refusing update.") url old_time new_time in
      `problem (msg, None)

let last_ticket = ref 0
let timeout_tickets = ref StringMap.empty
let take_ticket timeout =
  last_ticket := !last_ticket + 1;
  let ticket = string_of_int !last_ticket in
  timeout_tickets := StringMap.add ticket timeout !timeout_tickets;
  ticket

(* Turn a list of tasks into a list of their resolutions. *)
let rec collect = function
  | [] -> Lwt.return []
  | x :: xs ->
      lwt result =
        try_lwt
          lwt x = x in
          `success x |> Lwt.return
        with Safe_exception (msg, _) ->
          `problem msg |> Lwt.return in

      lwt results = collect xs in
      result :: results |> Lwt.return

class fetcher config trust_db (slave:Python.slave) =
  let system = config.system in

  let () =
    Python.register_handler "start-timeout" (function
      | [`String ticket] ->
          let timeout = StringMap.find ticket !timeout_tickets in
          timeout_tickets := StringMap.remove ticket !timeout_tickets;
          Lwt_timeout.start timeout;
          Lwt.return `Null
      | json -> raise_safe "start-timeout: invalid request: %s" (Yojson.Basic.to_string (`List json))
    ) in

  let download_url ?timeout_ticket ~hint url =
    let timeout_ticket =
      match timeout_ticket with
      | None -> `Null
      | Some ticket -> `String ticket in

    let request = `List [
      `String "download-url";
      `String url;
      `String hint;
      timeout_ticket;
    ] in

    slave#invoke_async request (function
      | `List [`String "success"; `String tmpname] -> `tmpfile tmpname
      | `String "aborted-by-user" -> `aborted_by_user
      | _ -> raise_safe "Invalid JSON response"
    ) in

  (** Check the GPG signatures on [tmpfile]. If any keys are missing, download and import them.
   * Returns a non-empty list of valid signatures, or a suitable error.
   * @param feed the feed we're trying to import.
   * @param url the URL we fetched it from (so if we get a feed from the mirror, we gets the keys there too)
   *)
  let download_missing_keys ~use_mirror feed url xml =
    lwt sigs, messages = G.verify system xml in

    if sigs = [] then
      raise_safe "No signatures found on feed %s!" feed;

    let any_imported = ref false in
    let aborted = ref false in
    let problem = ref None in

    let fetch key =
      let key_url =
        match use_mirror with
        | None ->
            let last_slash = String.rindex url '/' in
            String.sub url 0 (last_slash + 1) ^ (key ^ ".gpg")
        | Some mirror -> mirror ^ "/keys/" ^ (key ^ ".gpg") in

      log_info "Fetching key from %s" key_url;

      match_lwt download_url ~hint:feed key_url with
      | `tmpfile tmpfile ->
          let contents = U.read_file system tmpfile in
          system#unlink tmpfile;
          log_info "Importing key for feed '%s" feed;
          lwt () = G.import_key system contents in
          any_imported := true;
          Lwt.return ()
      | `aborted_by_user ->
          aborted := true;
          Lwt.return ()
      | `problem msg ->
          log_warning "Problem downloading key '%s': %s" key_url msg;
          problem := Some msg;
          Lwt.return () in

    (* Start a download for each missing key *)
    let missing_keys =
      sigs |> U.filter_map ~f:(function
        | G.ErrSig (G.UnknownKey key) -> Some (fetch key)
        | _ -> None
      ) in

    lwt () = Lwt.join missing_keys in

    if !aborted then Lwt.return `aborted_by_user
    else (
      (* Recalculate signatures if we imported any new keys. *)
      lwt sigs, messages =
        if !any_imported then G.verify system xml
        else Lwt.return (sigs, messages) in

      let have_valid =  sigs |> List.exists (function
        | G.ValidSig _ -> true
        | G.BadSig _ | G.ErrSig _ -> false
      ) in

      match have_valid, !problem with
      | false, Some problem -> `problem problem |> Lwt.return
      | _ -> `success (sigs, messages) |> Lwt.return
    ) in

  (** Return the timestampt of the oldest signature we trust on this feed, or None if we don't
   * trust any of them. *)
  let oldest_trusted_sig domain sigs =
    let oldest = ref None in
    sigs |> List.iter (function
      | G.ValidSig {G.fingerprint; G.timestamp} ->
          if trust_db#is_trusted ~domain fingerprint then (
            match !oldest with
            | Some old_best when timestamp > old_best -> ()
            | _ -> oldest := Some timestamp
          )
      | G.BadSig _ | G.ErrSig _ -> ()
    );
    !oldest in

  (** Import a downloaded feed into the cache. We've already checked that we trust the
   * signature by this point. *)
  let update_feed_from_network feed new_xml timestamp =
    let pretty_time = timestamp |> Unix.localtime |> U.format_time_pretty in
    let `remote_feed feed_url = feed in
    log_debug "Updating '%s' from network; modified at %s" feed_url pretty_time;

    (* Check the new XML is valid before adding it *)
    let new_root = `String (0, new_xml) |> Xmlm.make_input |> Q.parse_input (Some feed_url) in
    ignore @@ Feed.parse system new_root None;

    (* Load the old XML *)
    let cache_path = Feed_cache.get_save_cache_path config feed in
    let old_xml =
      if system#file_exists cache_path then
        Some (U.read_file system cache_path)
      else None in

    let update_last_checked_time () =
      if not config.dry_run then (
        Feed.update_last_checked_time config feed_url;
        log_info "Updated feed cache checked time for %s (modified %s)" feed_url pretty_time
      );
      `ok new_root |> Lwt.return in

    if old_xml = Some new_xml then (
      log_debug "No change";
      update_last_checked_time ()
    ) else if config.dry_run then (
      Dry_run.log "would cache feed %s as %s" feed_url cache_path;
      `ok new_root |> Lwt.return
    ) else (
      let save_new_xml () =
        system#atomic_write [Open_wronly; Open_binary] cache_path ~mode:0o644 (fun ch ->
          output_string ch new_xml
        );
        log_debug "Saved as %s" cache_path;
        update_last_checked_time () in

      (* Check the timestamp is newer than the old version *)
      match old_xml with
      | None -> save_new_xml ()
      | Some old_xml ->
          lwt old_sigs, warnings = G.verify system old_xml in
          match oldest_trusted_sig (Trust.domain_from_url feed_url) old_sigs with
          | None -> raise_safe "%s" warnings
          | Some old_modified when old_modified > timestamp ->
              `replay_attack (feed_url, old_modified, timestamp) |> Lwt.return
          | Some _ -> save_new_xml ()
    ) in

  (** We don't trust any of the signatures yet. Collect information about them and add the keys to the
      trust_db, possibly after confirming with the user. *)
  let confirm_keys feed sigs messages =
    let valid_sigs = U.filter_map sigs ~f:(function
      | G.ValidSig info -> Some info
      | G.BadSig _ | G.ErrSig _ -> None
    ) in

    if valid_sigs = [] then (
      let format_sig s = "\n- " ^ G.string_of_sig s in
      let extra =
        if messages = "" then ""
        else "\nMessages from GPG:\n" ^ messages in
      raise_safe "No valid signatures found on '%s'. Signatures:%s%s"
        feed (String.concat "" (List.map format_sig sigs)) extra
    );

    let json_sigs = valid_sigs |> List.map (fun info -> `String info.G.fingerprint) in
    let request = `List [`String "confirm-keys"; `String feed; `List json_sigs] in
    slave#invoke_async request (function
      | `Null -> ()
      | _ -> raise_safe "Invalid response"
    ) in

  (** We've just downloaded the new version of the feed to a temporary file. Imported it into the cache. *)
  let import_feed ~mirror_used feed url xml =
    let `remote_feed feed_url = feed in
    match_lwt download_missing_keys ~use_mirror:mirror_used feed_url url xml with
    | `problem msg -> raise_safe "Failed to download missing keys: %s" msg
    | `aborted_by_user -> Lwt.return `aborted_by_user
    | `success (sigs, messages) ->
        match oldest_trusted_sig (Trust.domain_from_url feed_url) sigs with
        | Some timestamp -> update_feed_from_network feed xml timestamp
        | None ->
            lwt () = confirm_keys feed_url sigs messages in
            match oldest_trusted_sig (Trust.domain_from_url feed_url) sigs with
            | Some timestamp -> update_feed_from_network feed xml timestamp
            | None -> Lwt.return `no_trusted_keys
    in

  (* Try to download the feed [feed] from URL [url] (which is typically the same, unless we're
   * using a mirror.
   * If present, start [timeout_waker] when the download actually starts (time spent queuing doesn't count). *)
  let download_and_import_feed_internal ~mirror_used ?timeout feed ~url =
    let `remote_feed feed_url = feed in

    let timeout_ticket =
      match timeout with
      | None -> None
      | Some timeout -> Some (take_ticket timeout) in

    if config.dry_run then
      Dry_run.log "downloading feed from %s" url;

    try_lwt
      match_lwt download_url ?timeout_ticket ~hint:feed_url url with
      | `aborted_by_user -> Lwt.return `aborted_by_user
      | `tmpfile tmpfile ->
          let xml = U.read_file system tmpfile in
          system#unlink tmpfile;
          import_feed ~mirror_used feed url xml
    with Safe_exception (msg, _) ->
      `problem msg |> Lwt.return in

  (* The primary failed (already reported). Wait for the mirror. *)
  let wait_for_mirror mirror =
    match_lwt mirror with
    (* We already warned; no need to raise an exception too, as the mirror download succeeded. *)
    | `ok result -> `update (result, None) |> Lwt.return
    | `aborted_by_user -> `aborted_by_user |> Lwt.return
    | `replay_attack _ ->
        log_info "Version from mirror is older than cached version; ignoring it";
        Lwt.return `no_update
    | `no_trusted_keys ->
        Lwt.return `no_update
    | `problem msg ->
        log_info "Mirror download failed: %s" msg;
        Lwt.return `no_update in

  let wait_for_primary primary : _ Lwt.t =
    (* Wait for the primary (we're already got a response or failure from the mirror) *)
    match_lwt primary with
    | #non_mirror_case as result -> get_response result |> Lwt.return
    | `problem msg -> `problem (msg, None) |> Lwt.return in

  let re_scheme_sep = Str.regexp_string "://" in

  let escape_slashes s = Str.global_replace U.re_slash "%23" s in

  (* The algorithm from 0mirror. *)
  let get_feed_dir feed =
    if String.contains feed '#' then (
      raise_safe "Invalid URL '%s'" feed
    ) else (
      let scheme, rest = U.split_pair re_scheme_sep feed in
      if not (String.contains rest '/') then
        raise_safe "Missing / in %s" feed;
      let domain, rest = U.split_pair U.re_slash rest in
      [scheme; domain; rest] |> List.iter (fun part ->
        if part = "" || U.starts_with part "." then
          raise_safe "Invalid URL '%s'" feed
      );
      String.concat "/" ["feeds"; scheme; domain; escape_slashes rest]
    ) in

  let get_mirror_url mirror feed_url resource =
    let parsed_url = Neturl.parse_url feed_url in
    if Neturl.url_host parsed_url = "localhost" then
      None
    else (
      match Neturl.url_scheme parsed_url with
      | "http" | "https" -> Some (mirror ^ "/" ^ (get_feed_dir feed_url) ^ "/" ^ resource)
      | _ -> None
    ) in

  object
    method download_and_import_feed (feed : [`remote_feed of feed_url]) : fetch_feed_response Lwt.t =
      let `remote_feed feed_url = feed in
      log_debug "download_and_import_feed %s" feed_url;

      if not config.dry_run then (
        Feed_cache.mark_as_checking config feed
      );
      
      let timeout_task, timeout_waker = Lwt.wait () in
      let timeout = Lwt_timeout.create 5 (fun () -> Lwt.wakeup timeout_waker `timeout) in

      let primary = download_and_import_feed_internal ~mirror_used:None feed ~timeout ~url:feed_url in
      let do_mirror_download () =
        try
          match config.mirror with
          | None -> None
          | Some mirror ->
              match get_mirror_url mirror feed_url "latest.xml" with
              | None -> None
              | Some mirror_url ->
                  Some (download_and_import_feed_internal ~mirror_used:(Some mirror) feed ~url:mirror_url)
        with ex ->
          log_warning ~ex "Error getting mirror URL for '%s" feed_url;
          None in

      (* Download just the upstream feed, unless it takes too long... *)
      match_lwt Lwt.choose [primary; timeout_task] with
      (* Downloaded feed within 5 seconds *)
      | #non_mirror_case as result -> get_response result |> Lwt.return
      | `problem msg -> (
          match do_mirror_download () with
          | None -> `problem (msg, None) |> Lwt.return
          | Some mirror -> `problem (msg, Some (wait_for_mirror mirror)) |> Lwt.return
      )
      | `timeout ->
          (* OK, maybe it's just being slow... *)
          log_info "Feed download from %s is taking a long time." feed_url;

          (* Start downloading from mirror... *)
          match do_mirror_download () with
          | None -> wait_for_primary primary
          | Some mirror ->
              (* Wait for a result from either *)
              lwt _ = Lwt.choose [primary; mirror] in

              match Lwt.state primary with
              | Lwt.Fail msg -> raise msg
              | Lwt.Sleep -> (
                  (* The mirror finished first *)
                  match_lwt mirror with
                  | `aborted_by_user ->
                      wait_for_primary primary
                  | `ok result ->
                      log_info "Mirror succeeded, but will continue to wait for primary";
                      `update (result, Some (wait_for_primary primary)) |> Lwt.return
                  | `replay_attack _ ->
                      log_info "Version from mirror is older than cached version; ignoring it";
                      wait_for_primary primary
                  | `no_trusted_keys ->
                      wait_for_primary primary
                  | `problem msg ->
                      log_info "Mirror download failed: %s" msg;
                      wait_for_primary primary
              )
              | Lwt.Return v -> (
                  (* The primary returned first *)
                  match v with
                  | #non_mirror_case as result ->
                      Lwt.cancel mirror;
                      get_response result |> Lwt.return
                  | `problem msg ->
                      `problem (msg, Some (wait_for_mirror mirror)) |> Lwt.return
              )

    (** Ensure all selections are cached, downloading any that are missing.
        If [distro] is given then distribution packages are also installed, otherwise
        they are ignored. *)
    method download_selections ?distro sels : [ `success | `aborted_by_user ] Lwt.t =
      if Selections.get_unavailable_selections config ?distro sels <> [] then (
        let opts = `Assoc [
          ("include-packages", `Bool (distro <> None));
        ] in

        let request : Yojson.Basic.json = `List [`String "download-selections"; opts] in

        lwt result =
          slave#invoke_async ~xml:sels request (function
            | `List dry_run_paths -> `success (List.map Yojson.Basic.Util.to_string dry_run_paths)
            | `String "aborted-by-user" -> `aborted_by_user
            | json -> raise_safe "Invalid JSON response '%s'" (Yojson.Basic.to_string json)
          ) in

        match result with
        | `aborted_by_user -> Lwt.return `aborted_by_user
        | `success dry_run_paths ->
            (* In --dry-run mode, the directories haven't actually been added, so we need to tell the
             * dryrun_system about them. *)
            if config.dry_run then (
              List.iter (fun name -> system#mkdir name 0o755) dry_run_paths
            );
            Lwt.return `success
      ) else (
        Lwt.return `success
      )
  end
