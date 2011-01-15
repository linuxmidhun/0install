#!/usr/bin/env python
from basetest import BaseTest
import sys
import unittest

sys.path.insert(0, '..')
from zeroinstall.injector import reader, model
from zeroinstall.injector.policy import Policy

import warnings
import logging
logger = logging.getLogger()
#logger.setLevel(logging.DEBUG)

class TestPolicy(BaseTest):
	def testSource(self):
		iface_cache = self.config.iface_cache
		warnings.filterwarnings("ignore", category = DeprecationWarning)

		foo = iface_cache.get_interface('http://foo/Binary.xml')
		reader.update(foo, 'Binary.xml')
		foo_src = iface_cache.get_interface('http://foo/Source.xml')
		reader.update(foo_src, 'Source.xml')
		compiler = iface_cache.get_interface('http://foo/Compiler.xml')
		reader.update(compiler, 'Compiler.xml')

		p = Policy('http://foo/Binary.xml', config = self.config)
		p.freshness = 0
		p.network_use = model.network_full
		p.recalculate()	# Deprecated
		assert p.implementation[foo].id == 'sha1=123'

		# Now ask for source instead
		p.requirements.source = True
		p.requirements.command = 'compile'
		p.recalculate()
		assert p.solver.ready, p.solver.get_failure_reason()
		assert p.implementation[foo].id == 'sha1=234'		# The source
		assert p.implementation[compiler].id == 'sha1=345'	# A binary needed to compile it

if __name__ == '__main__':
	unittest.main()
