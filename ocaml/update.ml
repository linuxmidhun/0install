(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** The "0install update" command *)

open Zeroinstall.General
open Options
open Support.Common

module R = Zeroinstall.Requirements
module Q = Support.Qdom
module F = Zeroinstall.Feed

let get_root_sel sels =
  let iface = ZI.get_attribute F.attr_interface sels in
  let is_root sel = ZI.get_attribute F.attr_interface sel = iface in
  match Q.find is_root sels with
  | Some sel -> sel
  | None -> raise_safe "Can't find a selection for the root (%s)!" iface

let get_newest options feed_provider reqs =
  let module I = Zeroinstall.Impl_provider in
  let impl_provider = new I.default_impl_provider options.config feed_provider in
  let (scope, _root_key) = Zeroinstall.Solver.get_root_requirements options.config reqs in
  let get_impls = impl_provider#get_implementations scope.Zeroinstall.Solver.scope_filter reqs.R.interface_uri in

  let best = ref None in
  let check_best items =
    ListLabels.iter items ~f:(fun impl ->
      match !best with
      | None -> best := Some impl
      | Some old_best when F.(impl.parsed_version > old_best.parsed_version) -> best := Some impl
      | Some _ -> ()
    ) in

  let candidates = get_impls ~source:reqs.R.source in
  check_best candidates.I.impls;
  check_best @@ List.map fst candidates.I.rejects;
  if not reqs.R.source then (
    (* Also report newer source versions *)
    let candidates = get_impls ~source:true in
    check_best candidates.I.impls;
    check_best @@ List.map fst candidates.I.rejects
  );
  !best

let check_replacement system = function
  | None -> ()
  | Some (feed, _) ->
      match feed.F.replacement with
      | None -> ()
      | Some replacement ->
          Support.Utils.print system "Warning: interface %s has been replaced by %s" feed.F.url replacement

let check_for_updates options reqs old_sels =
  let config = options.config in
  let distro = Lazy.force options.distro in
  let new_sels = Zeroinstall.Helpers.solve_and_download_impls config distro options.slave
                          reqs Zeroinstall.Helpers.Select_for_update ~refresh:true ~use_gui:options.gui in
  match new_sels with
  | None -> raise (System_exit 1)   (* Aborted by user *)
  | Some new_sels ->
      let config = options.config in
      let system = config.system in
      let print fmt = Support.Utils.print system fmt in
      let feed_provider = new Zeroinstall.Feed_cache.feed_provider options.config distro in
      check_replacement system @@ feed_provider#get_feed reqs.R.interface_uri;
      let root_sel = get_root_sel new_sels in
      let root_version = ZI.get_attribute F.attr_version root_sel in
      let changes = ref (Whatchanged.show_changes system old_sels new_sels) in
      if not !changes && Q.compare_nodes old_sels new_sels ~ignore_whitespace:true <> 0 then (
        changes := true;
        print "Updates to metadata found, but no change to version (%s)." root_version;
        log_debug "Old:\n%s\nNew:\n%s" (Q.to_utf8 old_sels) (Q.to_utf8 new_sels)
      );

      let () =
        match get_newest options feed_provider reqs with
        | None -> log_warning "Can't find any implementations! (BUG)"
        | Some best ->
            if best.F.parsed_version > Zeroinstall.Versions.parse_version root_version then (
              print "A later version (%s %s) exists but was not selected. Using %s instead."
                reqs.R.interface_uri (Zeroinstall.Versions.format_version best.F.parsed_version) root_version;
              if not config.help_with_testing && best.F.stability < Stable then
                print "To select \"testing\" versions, use:\n0install config help_with_testing True"
            ) else if not !changes then (
              print "No updates found. Continuing with version %s." root_version
            ) in

      if !changes then
        Some new_sels
      else
        None

let handle options flags args =
  let config = options.config in

  let background = ref false in
  let select_opts = ref [] in
  Support.Argparse.iter_options flags (function
    | #common_option as o -> Common_options.process_common_option options o
    | #select_option as o -> select_opts := o :: !select_opts
    | `Refresh -> log_warning "deprecated: update implies --refresh anyway"
    | `Background -> background := true
  );
  match args with
  | [arg] -> (
    let open Generic_select in
    match resolve_target config !select_opts arg with
    | (App (app, _old_reqs), reqs) ->
        let old_sels = Zeroinstall.Apps.get_selections_no_updates config app in
        let () =
          match check_for_updates options reqs old_sels with
          | Some new_sels -> Zeroinstall.Apps.set_selections config app new_sels ~touch_last_checked:true;
          | None -> () in
        Zeroinstall.Apps.set_requirements config app reqs
    | (Selections old_sels, reqs) -> ignore @@ check_for_updates options reqs old_sels
    | (Interface, reqs) ->
        (* Select once without downloading to get the old values *)
        let distro = Lazy.force options.distro in
        let feed_provider = new Zeroinstall.Feed_cache.feed_provider config distro in
        let (ready, result) = Zeroinstall.Solver.solve_for config feed_provider reqs in
        let old_sels = result#get_selections in
        if not ready then old_sels.Q.child_nodes <- [];
        ignore @@ check_for_updates options reqs old_sels
  )
  | _ -> raise (Support.Argparse.Usage_error 1)
