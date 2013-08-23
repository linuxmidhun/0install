(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Explaining why a solve failed. *)

open General
open Support.Common
module Qdom = Support.Qdom

module S = Solver.S

module SelMap = Map.Make (
  struct
    type t = (iface_uri * bool)
    let compare = compare
  end
)

type rejection_reason = [
  | Impl_provider.rejection
  | `FailsRestriction of Feed.restriction
  | `DepFailsRestriction of Feed.dependency * Feed.restriction
  | `MachineGroupConflict of Feed.implementation
  | `ConflictsInterface of iface_uri
  | `MissingCommand of string
  | `DiagnosticsFailure of string
]

type note =
  | UserRequested of Feed.restriction
  | ReplacesConflict of iface_uri
  | ReplacedByConflict of iface_uri
  | Restricts of iface_uri * Feed.implementation * Feed.restriction list
  | RequiresCommand of iface_uri * Feed.implementation * string
  | NoUsableCandidates of (Feed.implementation * rejection_reason) list
  | NoCandidatesMeetRestrictions of (Feed.implementation * rejection_reason) list

type interface_report = {
  sel : Feed.implementation option;
  notes : note list;
}

let format_restrictions r = String.concat ", " (List.map (fun r -> r#to_string) r)
let format_version impl = Versions.format_version impl.Feed.parsed_version

let spf = Printf.sprintf

let describe_problem impl = function
  | #Impl_provider.rejection as p -> Impl_provider.describe_problem impl p
  | `FailsRestriction r -> "Incompatible with restriction: " ^ r#to_string
  | `DepFailsRestriction (dep, restriction) -> spf "Requires %s %s" dep.Feed.dep_iface (format_restrictions [restriction])
  | `MachineGroupConflict other_impl ->
      let this_arch = default "BUG" impl.Feed.machine in
      let other_name = Feed.get_attr Feed.attr_from_feed other_impl in
      let other_arch = default "BUG" other_impl.Feed.machine in
      spf "Can't use %s with selection of %s (%s)" this_arch other_name other_arch
  | `ConflictsInterface other_iface -> spf "Conflicts with %s" other_iface
  | `MissingCommand command -> spf "No %s command" command
  | `DiagnosticsFailure msg -> spf "Reason for rejection unknown: %s" msg

let format_report buf (iface_uri, _source) report =
  let prefix = ref "- " in

  let add fmt =
    let do_add msg = Buffer.add_string buf !prefix; Buffer.add_string buf msg in
    Printf.ksprintf do_add fmt in

  let name_impl impl = Feed.get_attr Feed.attr_id impl in

  let () = match report.sel with
    | Some sel -> add "%s -> %s (%s)" iface_uri (format_version sel) (name_impl sel)
    | None -> add "%s -> (problem)" iface_uri in

  prefix := "\n    ";

  let show_rejections rejected =
    prefix := "\n      ";
    let by_version (a, _) (b, _) = Feed.(compare b.parsed_version a.parsed_version) in
    let rejected = List.sort by_version rejected in
    let i = ref 0 in
    let () =
      try
        ListLabels.iter rejected ~f:(fun (impl, problem) ->
          if !i = 5 then (add "..."; raise Exit);
          add "%s (%s): %s" (name_impl impl) (format_version impl) (describe_problem impl problem);
          i := !i + 1
        );
      with Exit -> () in
    prefix := "\n    " in

  ListLabels.iter report.notes ~f:(function
    | UserRequested r -> add "User requested %s" (format_restrictions [r])
    | ReplacesConflict old -> add "Replaces (and therefore conflicts with) %s" old
    | ReplacedByConflict replacement -> add "Replaced by (and therefore conflicts with) %s" replacement
    | Restricts (other_iface, impl, r) ->
        add "%s %s requires %s" other_iface (format_version impl) (format_restrictions r)
    | RequiresCommand (other_iface, impl, command) ->
        add "%s %s requires '%s' command" other_iface (format_version impl) command
    | NoUsableCandidates [] ->
        add "No known implementations at all"
    | NoUsableCandidates rejected ->
        add "No usable implementations:";
        show_rejections rejected
    | NoCandidatesMeetRestrictions rejected ->
        add "Rejected candidates:";
        show_rejections rejected
  );

  Buffer.add_string buf "\n"

exception Reject of rejection_reason

let get_failure_report result =
  let (root_scope, sat, impl_provider, impl_cache, root_req) = result#get_details in

  let impls =
    let map = ref SelMap.empty in

    let get_selected (key, candidates) =
      match candidates#get_clause () with
      | None -> ()    (* Not part of the (dummy) solution (can't happen?) *)
      | Some clause ->
          match S.get_selected clause with
          | None -> ()    (* Not part of the (dummy) solution *)
          | Some lit ->
              let sel = (
                match (S.get_varinfo_for_lit sat lit).S.obj with
                | Solver.SolverData.ImplElem impl ->
                    if impl.Feed.parsed_version = Versions.dummy then None else Some impl
                | _ -> assert false
              ) in
              map := SelMap.add key sel !map in

    List.iter get_selected @@ impl_cache#get_items ();
    !map in

  let examine_selection (iface_uri, source) sel =
    let notes = ref [] in
    let add note = notes := note :: !notes in

    (* Find all restrictions that are in play and affect this interface *)

    (* orig_impls is all the implementations passed to the SAT solver (these are the
       ones with a compatible OS, CPU, etc). They are sorted most desirable first. *)
    let {Impl_provider.replacement = our_replacement; Impl_provider.impls = orig_impls; Impl_provider.rejects} =
      impl_provider#get_implementations root_scope.Solver.scope_filter iface_uri ~source in

    let good_impls = ref orig_impls in
    let bad_impls = ref (rejects :> (Feed.implementation * rejection_reason) list) in

    let filter_impls get_problem =
      let old_good = List.rev !good_impls in
      good_impls := [];
      ListLabels.iter old_good ~f:(fun impl ->
        match get_problem impl with
        | None -> good_impls := impl :: !good_impls
        | Some problem -> bad_impls := (impl, problem) :: !bad_impls
      ) in

    (* Remove from [good_impls] anything that fails to meet these restrictions.
       Add removed items to [bad_impls], along with the cause. *)
    let apply_restrictions restrictions =
      ListLabels.iter restrictions ~f:(fun r ->
        filter_impls (fun impl ->
          if r#meets_restriction impl then None
          else Some (`FailsRestriction r)
        )
      ) in

    let reject_all reason =
      bad_impls := List.map (fun impl -> (impl, reason)) !good_impls @ !bad_impls;
      good_impls := []
    in

    let get_machine_group impl =
      match impl.Feed.machine with
      | None -> None
      | Some "src" -> None
      | Some m -> Some (Arch.get_machine_group m) in

    let required_machine_group = ref None in
    let example_machine_impl = ref None in		(* An example chosen impl with a machine type *)

    (* For each selected/dummy implementation... *)
    let check_other (other_uri, other_source) other_sel =
      (* Check for interface-level conflicts *)
      let {Impl_provider.replacement = other_replacement; Impl_provider.impls = _other_impls; Impl_provider.rejects = _} =
        impl_provider#get_implementations root_scope.Solver.scope_filter other_uri ~source:other_source in

      if other_replacement = Some iface_uri then (
        add (ReplacesConflict other_uri);
        if other_sel <> None then (
          reject_all (`ConflictsInterface other_uri);
        )
      );

      if our_replacement = Some other_uri then (
        add (ReplacedByConflict other_uri);
        if other_sel <> None then (
          reject_all (`ConflictsInterface other_uri);
        )
      );

      match other_sel with
      | None -> ()    (* If we didn't select an implementation then that can't be causing a problem *)
      | Some other_sel ->
          if !example_machine_impl = None then (
            required_machine_group := get_machine_group other_sel;
            if !required_machine_group <> None then
              example_machine_impl := Some other_sel
          );

          ListLabels.iter other_sel.Feed.props.Feed.requires ~f:(fun dep ->
            (* If it depends on us and has restrictions... *)
            if dep.Feed.dep_iface = iface_uri then (
              if dep.Feed.dep_restrictions <> [] then (
                (* Report the restriction *)
                add (Restricts (other_uri, other_sel, dep.Feed.dep_restrictions));

                (* Remove implementations incompatible with the other selections *)
                apply_restrictions dep.Feed.dep_restrictions
              );

              ListLabels.iter dep.Feed.dep_required_commands ~f:(fun command ->
                add (RequiresCommand (other_uri, other_sel, command));
                filter_impls (fun impl ->
                  if StringMap.mem command Feed.(impl.props.commands) then None
                  else Some (`MissingCommand command)
                )
              )
            )
          ) in
    SelMap.iter check_other impls;

    (* Check for user-supplied restrictions *)
    let () =
      let user =
        try Some (StringMap.find iface_uri root_scope.Solver.scope_filter.Impl_provider.extra_restrictions)
        with Not_found -> None in
      match user with
      | None -> ()
      | Some restriction ->
          add (UserRequested restriction);
          apply_restrictions [restriction]
    in

    if sel = None then (
      if (!good_impls = []) then
        add (NoUsableCandidates !bad_impls)
      else (
        let () =
          match root_req with
          | Solver.ReqCommand (root_command, root_iface, _source) when root_iface = iface_uri ->
              filter_impls (fun impl ->
                if StringMap.mem root_command Feed.(impl.props.commands) then None
                else Some (`MissingCommand root_command)
              )
          | _ -> () in

        (* Report on available implementations
           all_impls = all known implementations
           orig_impls = impls valid on their own (e.g. incompatible archs removed)
           good_impls = impls compatible with other selections used in this example *)
        (* Move all remaining good candidates to bad, with a reason. *)
        ListLabels.iter !good_impls ~f:(fun sel ->
          try
            let () =
              match !example_machine_impl with
              | None -> ()
              | Some example_machine_impl  ->
                  (* Could be an architecture problem *)
                  let this_machine_group = get_machine_group sel in
                  if this_machine_group <> None && this_machine_group <> !required_machine_group then
                    raise (Reject (`MachineGroupConflict example_machine_impl)) in

            (* Check if our requirements conflict with an existing selection *)
            ListLabels.iter sel.Feed.props.Feed.requires ~f:(fun dep ->
              let dep_selection =
                (* Note: will need updating if we ever allow dependencies on source *)
                try SelMap.find (dep.Feed.dep_iface, false) impls
                with Not_found -> None in
              match dep_selection with
              | Some dep_selection ->
                  ListLabels.iter dep.Feed.dep_restrictions ~f:(fun r ->
                    if not @@ r#meets_restriction dep_selection then
                      raise (Reject (`DepFailsRestriction (dep, r)))
                  )
              | None -> ()
            );

            (* Give up - report the internal SAT reason for debugging. *)
            let internal_error =
              match impl_cache#peek (iface_uri, source) with
              | None -> "BUG: no var for impl!"
              | Some candidates ->
                  match candidates#get_clause () with
                  | None -> "BUG: no clause!"
                  | Some clause ->
                      match S.get_selected clause with
                      | None -> "BUG: no var for impl!"
                      | Some lit -> S.explain_reason sat lit in
            raise (Reject (`DiagnosticsFailure internal_error))

(*
                    varinfo = problem.get_varinfo_for_lit(var)
                    reason = "Hard to explain. Internal reason: {reason} => {assignment}".format(
                            reason = varinfo.reason,
                            assignment = varinfo)
*)

          with Reject reason ->
            bad_impls := (sel, reason) :: !bad_impls
        );
        add (NoCandidatesMeetRestrictions !bad_impls)
      )
    );

    {sel; notes = List.rev !notes} in

  SelMap.mapi examine_selection impls

(** Return a message explaining why the solve failed. *)
let get_failure_reason config result =
  let reasons = get_failure_report result in

  let buf = Buffer.create 1000 in
  Buffer.add_string buf "Can't find all required implementations:\n";
  SelMap.iter (format_report buf) reasons;
  if config.network_use = Offline then
    Buffer.add_string buf "Note: 0install is in off-line mode\n";
  Buffer.sub buf 0 (Buffer.length buf - 1)

exception Return of string

let return fmt =
  let do_return msg = raise (Return msg) in
  Printf.ksprintf do_return fmt

(* XXX Duplicated! *)
let make_selection_map sels =
  let add_selection m sel =
    StringMap.add (ZI.get_attribute "interface" sel) sel m
  in ZI.fold_left ~f:add_selection StringMap.empty sels "selection"

let get_id sel =
  let feed =
    match ZI.get_attribute_opt Feed.attr_from_feed sel with
    | Some feed -> feed
    | None -> ZI.get_attribute Feed.attr_interface sel in
  let id = ZI.get_attribute Feed.attr_id sel in
  (feed, id)

(* We are able to select the specimen, but we preferred not to. Explain why. 
   [results] is the selections with the constraint.
   [old_sels] are the selections we get with an unconstrained solve.
 *)
let justify_preference results wanted (q_iface, q_feed, q_id) ~old_sels ~compare candidates =
  let sels = results#get_selections () in

  let index = make_selection_map sels in

  let actual_selection =
    let is_our_iface sel = ZI.tag sel = Some "selection" && ZI.get_attribute Feed.attr_interface sel = q_iface in
    try Some (List.find is_our_iface old_sels.Qdom.child_nodes)
    with Not_found -> None in

  let () =
    match actual_selection, compare with
    | Some actual_selection, Some compare -> (
        let wanted_id = (q_feed, q_id) in
        let actual_id = get_id actual_selection in

        (* Was impl actually selected anyway? *)
        if get_id actual_selection = (q_feed, q_id) then
          return "%s was selected as the preferred version." wanted;

        let wanted_impl = ref None in
        let actual_impl = ref None in

        ListLabels.iter candidates ~f:(fun impl ->
          let id = Feed.get_id impl in
          if id = wanted_id then
            wanted_impl := Some impl;
          if id = actual_id then
            actual_impl := Some impl
        );

        match !wanted_impl, !actual_impl with
        | _, None -> failwith "Didn't find actual impl!"
        | None, _ -> failwith "Didn't find wanted impl!"
        | Some wanted_impl, Some actual_impl ->
            (* Was impl ranked below the selected version? *)
            let (result, reason) = compare wanted_impl actual_impl in

            if result > 0 then (
              let reason_msg =
                let open Impl_provider in
                match reason with
                | PreferAvailable -> "is locally available"
                | PreferDistro    -> "native packages are preferred"
                | PreferID        -> "better ID (tie-breaker)"
                | PreferLang      ->  "natural languages we understand are preferred"
                | PreferMachine   -> "better CPU match"
                | PreferNonRoot   -> "packages that don't require admin access to install are preferred"
                | PreferOS        -> "better OS match"
                | PreferStability -> "more stable versions preferred"
                | PreferVersion   -> "newer versions are preferred" in

              (* If they both have the same version number, include the ID in the message too. *)
              let wanted_version = Feed.(get_attr attr_version wanted_impl) in
              let actual_version = Feed.(get_attr attr_version actual_impl) in

              let truncate (_feed, id) =
                if String.length id < 18 then id
                else String.sub id 0 16 ^ "..." in

              let (wanted, actual) =
                if wanted_version = actual_version then
                  (spf "%s (%s)" wanted_version @@ truncate wanted_id,
                   spf "%s (%s)" actual_version @@ truncate actual_id)
                else
                  (wanted_version, actual_version) in

              return "%s is ranked lower than %s: %s" wanted actual reason_msg
            )
    )
    | _ -> () in

  let used_impl = actual_selection <> None in

  (* Impl is selectable and ranked higher than the selected version. Selecting it would cause
      a problem elsewhere. Or, its interface just isn't needed. *)
  let changes = ref [] in
  let add fmt =
    let do_add msg = changes := msg :: !changes in
    Printf.ksprintf do_add fmt in

  ZI.iter old_sels ~f:(fun old_sel ->
    let old_iface = ZI.get_attribute Feed.attr_interface old_sel in
    if old_iface <> q_iface || not used_impl then (
      try
        let new_sel = StringMap.find old_iface index in
        let old_version = ZI.get_attribute Feed.attr_version old_sel in
        let new_version = ZI.get_attribute Feed.attr_version new_sel in
        if old_version <> new_version then (
          add "%s: %s to %s") old_iface old_version new_version
        else (
          let old_id = ZI.get_attribute Feed.attr_id old_sel in
          let new_id = ZI.get_attribute Feed.attr_id new_sel in
          if old_id <> new_id then
            add "%s: %s to %s" old_iface old_id new_id
        )
      with Not_found ->
        add "%s: no longer used" old_iface
    )
  );

  let changes_text =
    if !changes <> [] then
      "\n\nThe changes would be:\n\n" ^ (String.concat "\n" (List.rev !changes))
    else "" in

  if used_impl then
    return "%s is selectable, but using it would produce a less optimal solution overall.%s" wanted changes_text
  else
    return "If %s were the only option, the best available solution wouldn't use it.%s" wanted changes_text
    ;;
(** Run a solve with impl_id forced to be selected, and explain why it wasn't (or was)
    selected in the normal case. *)
let justify_decision config feed_provider requirements (q_iface, q_feed, q_id) =
  let (scope, root_req) = Solver.get_root_requirements config requirements in

  (* Note: there's a slight mismatch between the diagnostics system (which assumes each interface is used either for
     source or binaries, but not both, and the current implementation of the solver. *)

  let wanted = ref @@ spf "%s %s" q_iface q_id in

  let candidates = ref [] in

  (* Wrap default_impl_provider so that it only returns our impl for [q_iface]. If impl isn't usable,
     we return early. *)
  let impl_provider =
    let open Impl_provider in
    object
      inherit default_impl_provider config ~watch_iface:q_iface feed_provider as super

      method! get_implementations scope_filter requested_iface ~source:want_source =
        let c = super#get_implementations scope_filter requested_iface ~source:want_source in
        if requested_iface <> q_iface then c
        else (
          candidates := c.impls;
          let is_ours candidate = Feed.(
            (get_attr attr_id candidate) = q_id &&
              (get_attr attr_from_feed candidate) = q_feed
          )in
          try
            let our_impl = List.find is_ours c.impls in
            wanted := spf "%s %s" q_iface Feed.(get_attr attr_version our_impl);
            {impls = [our_impl]; replacement = c.replacement; rejects = []}
          with Not_found ->
            try
              let (our_impl, problem) = List.find (fun (cand, _) -> is_ours cand) c.rejects in
              wanted := spf "%s %s" q_iface Feed.(get_attr attr_version our_impl);
              return "%s cannot be used (regardless of other components): %s" !wanted (Impl_provider.describe_problem our_impl problem)
            with Not_found -> return "Implementation to consider (%s) does not exist!" !wanted

        )
    end in

  (* Could a selection involving impl even be valid? *)
  try
    match Solver.do_solve impl_provider scope root_req ~closest_match:false with
    | Some result ->
        let (ready, actual_selections) = Solver.solve_for config feed_provider requirements in
        assert ready;   (* If we can solve we a constraint, we can solve without. *)
        justify_preference result !wanted (q_iface, q_feed, q_id) ~old_sels:(actual_selections#get_selections ()) ~compare:impl_provider#get_watched_compare !candidates
    | None ->
        match Solver.do_solve impl_provider scope root_req ~closest_match:true with
        | None -> failwith "No solution, even with closest_match!"
        | Some result ->
            return "There is no possible selection using %s.\n%s" !wanted @@ get_failure_reason config result
  with Return x -> x
