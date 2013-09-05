"""
Convenience routines for performing common operations.
@since: 0.28
"""

# Copyright (C) 2009, Thomas Leonard
# See the README file for details, or visit http://0install.net.

from __future__ import print_function

import os
from zeroinstall import SafeException, logger
from zeroinstall.support import tasks

DontUseGUI = object()

def should_use_gui(use_gui):
	if use_gui is False:
		return False

	if not os.environ.get('DISPLAY', None):
		if use_gui is None:
			return False
		else:
			raise SafeException("Can't use GUI because $DISPLAY is not set")

	from zeroinstall.gui import main
	if main.gui_is_available(use_gui):
		return True

	if use_gui is None:
		return False
	else:
		raise SafeException("No GUI available")

def get_selections_gui(iface_uri, gui_args, test_callback = None, use_gui = True):
	"""Run the GUI to choose and download a set of implementations.
	The user may ask the GUI to submit a bug report about the program. In that case,
	the GUI may ask us to test it. test_callback is called in that case with the implementations
	to be tested; the callback will typically call L{zeroinstall.injector.run.test_selections} and return the result of that.
	@param iface_uri: the required program, or None to show just the preferences dialog
	@type iface_uri: str
	@param gui_args: any additional arguments for the GUI itself
	@type gui_args: [str]
	@param test_callback: function to use to try running the program
	@type test_callback: L{zeroinstall.injector.selections.Selections} -> str
	@param use_gui: if True, raise a SafeException if the GUI is not available. If None, returns DontUseGUI if the GUI cannot be started. If False, returns DontUseGUI always. (since 1.11)
	@type use_gui: bool | None
	@return: the selected implementations
	@rtype: L{zeroinstall.injector.selections.Selections}
	@since: 0.28"""
	if not should_use_gui(use_gui):
		return DontUseGUI

	if use_gui is True:
		gui_args = ['-g'] + gui_args
	assert iface_uri is not None
	gui_args = gui_args + ['--', iface_uri]

	from zeroinstall.gui import main
	return main.run_gui(gui_args)		# XXX: cancel?

	if 0: # XXX
		if dom.getAttribute('run-test'):
			logger.info("Testing program, as requested by GUI...")
			if test_callback is None:
				output = b"Can't test: no test_callback was passed to get_selections_gui()\n"
			else:
				output = test_callback(sels)
			logger.info("Sending results to GUI...")
			output = ('Length:%8x\n' % len(output)).encode('utf-8') + output
			logger.debug("Sending: %s", repr(output))
			while output:
				sent = cli.send(output)
				output = output[sent:]
			continue

def ensure_cached(uri, command = 'run', config = None):
	"""Ensure that an implementation of uri is cached.
	If not, it downloads one. It uses the GUI if a display is
	available, or the console otherwise.
	@param uri: the required interface
	@type uri: str
	@type command: str
	@return: the selected implementations, or None if the user cancelled
	@rtype: L{zeroinstall.injector.selections.Selections}"""
	from zeroinstall.injector.driver import Driver

	if config is None:
		from zeroinstall.injector.config import load_config
		config = load_config()

	from zeroinstall.injector.requirements import Requirements
	requirements = Requirements(uri)
	requirements.command = command

	d = Driver(config, requirements)

	if d.need_download() or not d.solver.ready:
		finished = get_selections_gui(uri, ['--command', command], use_gui = None)
		if finished != DontUseGUI:
			tasks.wait_for_blocker(finished)
			return finished.gui_result
		done = d.solve_and_download_impls()
		tasks.wait_for_blocker(done)

	return d.solver.selections
