(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Parsing command-line arguments *)

val common_options :
  ([> `DryRun
    | `Help
    | `UseGUI of Support.Common.yes_no_maybe
    | `Verbose
    | `WithStore of Support.Common.filepath ],
   Options.zi_arg_type)
  Support.Argparse.opt_spec list

type subcommand =
   < handle : Options.global_settings ->
              Support.Argparse.raw_option list ->
              Zeroinstall.General.iface_uri list -> unit;
     help : string;
     options : (Options.zi_option, Options.zi_arg_type) Support.Argparse.opt_spec
               list >
val subcommands : (string * subcommand) list

val no_command : subcommand
val set_of_option_names : ('a, 'b) Support.Argparse.opt_spec list -> Support.Common.StringSet.t
val handle : Zeroinstall.General.config -> string list -> unit

val spec : (Options.zi_option, Options.zi_arg_type) Support.Argparse.argparse_spec
val get_default_options : Zeroinstall.General.config -> Options.global_settings
