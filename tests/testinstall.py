#!/usr/bin/env python
from basetest import BaseTest, TestStores
import sys, tempfile, os
from StringIO import StringIO
import unittest

sys.path.insert(0, '..')
from zeroinstall import cmd, SafeException
from zeroinstall.injector import model, selections, qdom, reader

class TestInstall(BaseTest):
	def run_0install(self, args):
		old_stdout = sys.stdout
		old_stderr = sys.stderr
		try:
			sys.stdout = StringIO()
			sys.stderr = StringIO()
			ex = None
			try:
				cmd.main(args, config = self.config)
			except NameError:
				raise
			except SystemExit:
				pass
			except TypeError:
				raise
			except AttributeError:
				raise
			except AssertionError:
				raise
			except Exception, ex:
				pass
			out = sys.stdout.getvalue()
			err = sys.stderr.getvalue()
			if ex is not None:
				err += str(ex.__class__)
		finally:
			sys.stdout = old_stdout
			sys.stderr = old_stderr
		return (out, err)

	def testHelp(self):
		out, err = self.run_0install([])
		assert out.lower().startswith("usage:")
		assert 'add-feed' in out
		assert '--version' in out
		assert not err, err

		out2, err = self.run_0install(['--help'])
		assert not err, err
		assert out2 == out

		out, err = self.run_0install(['--version'])
		assert 'Thomas Leonard' in out
		assert not err, err

		out, err = self.run_0install(['foobar'])
		assert 'Unknown sub-command' in err, err

	def testSelect(self):
		out, err = self.run_0install(['select'])
		assert out.lower().startswith("usage:")
		assert '--xml' in out

		out, err = self.run_0install(['select', 'Local.xml'])
		assert not err, err
		assert 'Version: 0.1' in out

		local_uri = model.canonical_iface_uri('Local.xml')
		out, err = self.run_0install(['select', 'Local.xml'])
		assert not err, err
		assert 'Version: 0.1' in out

		out, err = self.run_0install(['select', 'Local.xml', '--xml'])
		sels = selections.Selections(qdom.parse(StringIO(str(out))))
		assert sels.selections[local_uri].version == '0.1'

		out, err = self.run_0install(['select', 'selections.xml'])
		assert not err, err
		assert 'Version: 1\n' in out
		assert '(not cached)' in out

	def testDownload(self):
		out, err = self.run_0install(['download', 'Local.xml', '--show'])
		assert not err, err
		assert 'Version: 0.1' in out

		out, err = self.run_0install(['download', 'Local.xml', '--show', '--with-store=/foo'])
		assert not err, err
		assert self.config.stores.stores[-1].dir == '/foo'

		out, err = self.run_0install(['download', '--offline', 'selections.xml'])
		assert 'Would download' in err
		self.config.network_use = model.network_full

		self.config.stores = TestStores()
		digest = 'sha1=3ce644dc725f1d21cfcf02562c76f375944b266a'
		self.config.fetcher.allow_download(digest)
		out, err = self.run_0install(['download', 'Hello.xml', '--show'])
		assert not err, err
		assert self.config.stores.lookup_any([digest]).startswith('/fake')
		assert 'Version: 1\n' in out

		out, err = self.run_0install(['download', '--offline', 'selections.xml', '--show'])
		assert '/fake_store' in out
		self.config.network_use = model.network_full

	def testDownloadSelections(self):
		self.config.stores = TestStores()
		digest = 'sha1=3ce644dc725f1d21cfcf02562c76f375944b266a'
		self.config.fetcher.allow_download(digest)
		hello = reader.load_feed('Hello.xml')
		self.config.fetcher.allow_feed_download('http://example.com:8000/Hello.xml', hello)
		out, err = self.run_0install(['download', 'selections.xml', '--show'])
		assert not err, err
		assert self.config.stores.lookup_any([digest]).startswith('/fake')
		assert 'Version: 1\n' in out

if __name__ == '__main__':
	unittest.main()
