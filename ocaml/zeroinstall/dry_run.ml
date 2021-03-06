(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Helpers for --dry-run mode. *)

open Support.Common

(** Log a message saying what we would have done. *)
let log fmt =
  let do_print msg =
    print_endline @@ "[dry-run] " ^ msg in
  Printf.ksprintf do_print fmt

(** Wrap a system and prevent all changes (writes). This is used with --dry-run to prevent accidents. *)
class dryrun_system (underlying:system) =
  let reject msg = raise_safe "Bug: '%s' called in --dry-run mode" msg in
  object (_ : #system)
    val mutable fake_dirs = StringMap.empty

    (* Read-only operations: pass though *)
    method argv = underlying#argv
    method print_string = underlying#print_string
    method time = underlying#time
    method with_open_in = underlying#with_open_in
    method readdir = underlying#readdir
    method lstat = underlying#lstat
    method stat = underlying#stat
    method reap_child = underlying#reap_child
    method waitpid = underlying#waitpid
    method getcwd = underlying#getcwd
    method getenv = underlying#getenv
    method environment = underlying#environment
    method readlink = underlying#readlink
    method platform = underlying#platform
    method running_as_root = underlying#running_as_root

    method file_exists path =
      if underlying#file_exists path then true
      else (
        let dir = Filename.dirname path in
        let base = Filename.basename path in

        try StringSet.mem base (StringMap.find dir fake_dirs)
        with Not_found -> false
      )

    (* We allow this as we may be falling back to Python or running some helper.
       For places where it matters (e.g. actually running the target program), the caller should handle it. *)
    method exec = underlying#exec
    method create_process = underlying#create_process

    (* Trivial operations: ignore *)
    method set_mtime _path _mtime = ()
    method chmod _path _mode      = ()

    (* Keep track of the directories we would have created, since we often check them soon afterwards. *)
    method mkdir path _mode =
      let dir = Filename.dirname path in
      let base = Filename.basename path in

      let dir_entries =
        try StringMap.find dir fake_dirs
        with Not_found -> StringSet.empty in

      fake_dirs <- StringMap.add dir (StringSet.add base dir_entries) fake_dirs

    (* Interesting operations: log and skip *)
    method unlink path      = log "rm %s" path
    method atomic_hardlink ~link_to ~replace = log "ln %s %s" link_to replace
    method rmdir path       = log "rmdir %s" path
    method spawn_detach ?(search_path=false) ?env:_ argv = ignore search_path; log "would spawn background process: %s" (String.concat " " argv)

    (* Complex operations: reject (caller should handle specially) *)
    method with_open_out = reject "with_open_out"
    method atomic_write = reject "atomic_write"
  end
