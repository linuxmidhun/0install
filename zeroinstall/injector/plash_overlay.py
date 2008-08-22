import os, sys
import plash
from plash import process, pola_run_args, namespace
from logging import info

def execute_with_overlay(prog_path, prog_args, overlays):
	prog_path = str(prog_path)	# unicode -> str
	prog_args = map(str, prog_args)

	proc = plash.process.ProcessSpecWithNamespace()
	proc.setcmd(prog_path, *prog_args)
	proc.env = os.environ.copy()
	setup = pola_run_args.ProcessSetup(proc)
	root = plash.env.get_root_dir()
	proc.cwd_path = os.getcwd()

	# Start with the normal filesystem...
	proc.get_namespace().resolve_populate(root, '/', flags = namespace.FS_SLOT_RWC)

	# Replace each mount point with a union of the mount-point and the previous contents
	#overlays = []
	for mount_point, src in overlays:
		info("Union %s onto %s...", src, mount_point)
		proc.get_namespace().attach_at_path(mount_point,
			namespace.make_union_dir(
				namespace.resolve_obj(root, mount_point),
				namespace.resolve_obj(root, src)
		))
	forwarders = setup.grant_proxy_terminal_access()
	#setup.enable_logging(None)
	pid = proc.spawn()
	info("Starting Plash mainloop...")
	plash.mainloop.run_server()
	info("Waiting for %d ...", pid)
	child_pid, status = os.waitpid(pid, 0)
	info("Child exited with status %d", status)
	assert child_pid == pid
	sys.exit(status)
