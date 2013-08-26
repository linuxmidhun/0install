(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

open General
open Support.Common

module A = Async     (* TODO: move to support *)
module U = Support.Utils

(** Manages the process of downloading feeds during a solve.
    We use the solver to get the current best solution and the set of feeds it queried.
    We download any missing feeds and update any out-of-date ones, resolving each time
    we have more information. *)

type state = {
  mutable downloads_finished : StringSet.t;       (* URLs of downloads, successful or otherwise *)
  mutable downloads_in_progress : unit A.promise StringMap.t;
}

(** Run the solver, then download any feeds that are missing or that need to be
    updated. Each time a new feed is imported into the cache, the solver is run
    again, possibly adding new downloads.

    Note: if we find we need to download anything, we will refresh everything.

    @param force re-download all feeds, even if we're ready to run (implies update_local)
    @param update_local fetch PackageKit feeds even if we're ready to run *)
let solve_with_downloads config fetcher distro requirements ~force ~update_local =
  let force = ref force in
  let state = {
    downloads_finished = StringSet.empty;
    downloads_in_progress = StringMap.empty;
  } in

  let already_seen url =
    (StringSet.mem url state.downloads_finished) || (StringMap.mem url state.downloads_in_progress) in

  let forget_feed url =
    state.downloads_finished <- StringSet.remove url state.downloads_finished;
    state.downloads_in_progress <- StringMap.remove url state.downloads_in_progress in

  (* There are three cases:
     1. We want to run immediately if possible. If not, download all the information we can.
        (force = False, update_local = False)
     2. We're in no hurry, but don't want to use the network unnecessarily.
        We should still update local information (from PackageKit).
        (force = False, update_local = True)
     3. The user explicitly asked us to refresh everything.
        (force = True) *)

  let (final_result, final_resolver) = A.promise_pair () in

  let rec add_download url promise =
    log_info "Starting download of feed '%s'" url;
    state.downloads_in_progress <- StringMap.add url promise state.downloads_in_progress;
    A.when_resolved_ignored promise (fun resolution ->
      log_info "Finished download of feed '%s'" url;
      let () =
        match resolution with
        | `Broken ex -> log_warning ~ex "Feed download %s failed" url
        | `Fulfilled () -> () in
      (* Move download from in_progress to finished *)
      state.downloads_finished <- StringSet.add url state.downloads_finished;
      state.downloads_in_progress <- StringMap.remove url state.downloads_in_progress;
      (* Recalculate. Will resolve [final_result] if we're the last download. *)
      next ~try_quick_exit:false
    )
  and next ~try_quick_exit =
    (* Create a new cache each time. We're waiting on the network, so it's not speed critical, and
       it avoids having to keep track of what needs updating. *)
    let feed_provider = new Feed_cache.feed_provider config distro in

    (* Called once at the start, and once for every feed that downloads (or fails to download). *)
    let result = Solver.solve_for config feed_provider requirements in

    (* for w in self.watchers: w() *)

    match result with
    | (true, _) when try_quick_exit ->
        assert (StringMap.is_empty state.downloads_in_progress);
        A.fulfill final_resolver result
    | (ready, _) ->
        if not ready then force := true;

        (* For each remote feed used which we haven't seen yet, start downloading it. *)
        if !force && config.network_use <> Offline then (
          ListLabels.iter feed_provider#get_feeds_used ~f:(fun f ->
            if not (already_seen f) && not (Feed_cache.is_local_feed f) then (
              let download = fetcher#download_and_import_feed f in

              (* On success, we also need to refetch any "distribution" feed that depends on this one *)
              add_download f @@ A.when_fulfilled download (fun () ->
                forget_feed ("distribution:" ^ f);
                (* (we will now refresh, which will start a package-kit update *)
              )
            )
          )
        );

        (* Check for extra (uninstalled) local distro candidates. *)
        if !force || update_local then (
          ListLabels.iter feed_provider#get_feeds_used ~f:(fun f ->
            match feed_provider#get_feed f with
            | None -> ()
            | Some (master_feed, _) ->
                let f = "distribution:" ^ f in
                if not (already_seen f) then
                    add_download f @@ distro#check_for_candidates master_feed
          )
        );

        if StringMap.is_empty state.downloads_in_progress then (
          if config.network_use = Offline then
            log_info "Can't choose versions and in off-line mode, so aborting";
          A.fulfill final_resolver result
        )
        (* else one of the downloads in progress will call us again *)
  in
  next ~try_quick_exit:(not (!force || update_local)); final_result
