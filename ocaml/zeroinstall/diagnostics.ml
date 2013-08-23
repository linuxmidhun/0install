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
  | `UnknownReason
]

type note =
  | UserRequested of Feed.restriction
  | ReplacesConflict of iface_uri
  | ReplacedByConflict of iface_uri
  | Restricts of iface_uri * Feed.implementation * Feed.restriction list
  | RequiresCommand of iface_uri * Feed.implementation * string
  | NoValidCandidates of (Feed.implementation * rejection_reason) list

type interface_report = {
  sel : Feed.implementation option;
  notes : note list;
}

let format_restrictions r = String.concat ", " (List.map (fun r -> r#to_string) r)
let format_version impl = Versions.format_version impl.Feed.parsed_version

let spf = Printf.sprintf

let describe_problem impl = function
  | #Impl_provider.rejection as p -> Impl_provider.describe_problem impl p
  | `FailsRestriction r -> "Incompatible with restriction " ^ r#to_string
  | `DepFailsRestriction (dep, restriction) -> spf "Requires %s %s" dep.Feed.dep_iface (format_restrictions [restriction])
  | `MachineGroupConflict other_impl ->
      let this_arch = default "BUG" impl.Feed.machine in
      let other_name = Feed.get_attr Feed.attr_from_feed other_impl in
      let other_arch = default "BUG" other_impl.Feed.machine in
      spf "Can't use %s with selection of %s (%s)" this_arch other_name other_arch
  | `ConflictsInterface other_iface -> spf "Conflicts with %s" other_iface
  | `UnknownReason -> "Reason for rejection unknown"

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

  ListLabels.iter report.notes ~f:(function
    | UserRequested r -> add "User requested %s" (format_restrictions [r])
    | ReplacesConflict old -> add "Replaces (and therefore conflicts with) %s" old
    | ReplacedByConflict replacement -> add "Replaced by (and therefore conflicts with) %s" replacement
    | Restricts (other_iface, impl, r) ->
        add "%s %s requires %s" other_iface (format_version impl) (format_restrictions r)
    | RequiresCommand (other_iface, impl, command) ->
        add "%s %s requires '%s' command" other_iface (format_version impl) command
    | NoValidCandidates [] ->
        add "No known implementations at all"
    | NoValidCandidates rejected ->
        add "No implementations satisfy the restrictions:";
        prefix := "\n      ";
        let by_version (a, _) (b, _) = Feed.(compare b.parsed_version a.parsed_version) in
        let rejected = List.sort by_version rejected in
        ListLabels.iter rejected ~f:(fun (impl, problem) ->
          add "%s (%s): %s" (name_impl impl) (format_version impl) (describe_problem impl problem)
        );
        prefix := "\n    ";
  );

  Buffer.add_string buf "\n"

exception Reject of rejection_reason

(** Return a message explaining why the solve failed. *)
let get_failure_reason result =
(*   let sels = result#get_selections () in *)
  let (root_scope, sat, impl_provider, impl_cache) = result#get_details in

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

    (* Remove from [good_impls] anything that fails to meet these restrictions.
       Add removed items to [bad_impls], along with the cause. *)
    let apply_restrictions restrictions =
      ListLabels.iter restrictions ~f:(fun r ->
        let old_good = List.rev !good_impls in
        good_impls := [];
        ListLabels.iter old_good ~f:(fun impl ->
          if r#meets_restriction impl then
            good_impls := impl :: !good_impls
          else
            bad_impls := (impl, `FailsRestriction r) :: !bad_impls
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
                add (RequiresCommand (other_uri, other_sel, command))
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

  (* TODO
          var = self._iface_to_vars[iface].get(i, None)
          if var is None:
                  reason = "BUG: no var for impl!"
          else:
                  varinfo = problem.get_varinfo_for_lit(var)
                  reason = "Hard to explain. Internal reason: {reason} => {assignment}".format(
                          reason = varinfo.reason,
                          assignment = varinfo)
  *)
          raise (Reject `UnknownReason)
        with Reject reason ->
          bad_impls := (sel, reason) :: !bad_impls
      );
      add (NoValidCandidates !bad_impls)
    );

    {sel; notes = List.rev !notes} in

  let reasons = SelMap.mapi examine_selection impls in

  let buf = Buffer.create 1000 in
  SelMap.iter (format_report buf) reasons;
  Buffer.contents buf
