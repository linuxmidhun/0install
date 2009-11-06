from zeroinstall import _
from zipfile import ZipFile, ZipInfo
from logging import info
import os
import struct
import calendar

def _zipinfo_is_executable(zio):
	unix_system = 3
	if zio.create_system != unix_system:
		return False
	user_execute_flag = 0x0040
	if zio.external_attr >> 16 & user_execute_flag:
		return True
	else:
		return False

def _zipinfo_extra_parse(zio):
	position = 0
	extra_list = []
	while position < len(zio.extra):
		header_id, header_data_size = struct.unpack_from('<2sH', zio.extra, position)
		position += 4
		data = zio.extra[position:position + header_data_size]
		position += header_data_size
		extra_list += [(header_id, data)]
	return extra_list

def _zipinfo_unix_time(zio):
	extra_list = _zipinfo_extra_parse(zio)
	data = None
	for i, j in extra_list:
		if i == 'UT':
			data = j
	if data  == None:
		info(_('There are not a UT data in the zipinfo: %s'), zio.filename)
		return None
	mtime_flag = 0x01
	flags, = struct.unpack('b', data[0])
	if not (flags & mtime_flag):
		info(_('The zipinfo does not have mtime: %s'), zio.filename)
		return None
	mtime, = struct.unpack('<i', data[1:5])
	return mtime

class InfoZipInfo(ZipInfo):
	__slots__ = (
		'is_executable',
		'unix_time',
		)
	def __init__(self, zi):
		self._copy(zi)
		self.is_executable = _zipinfo_is_executable(self)
		self.unix_time = _zipinfo_unix_time(self)
	def _copy(self, zi):
		self.orig_filename = zi.orig_filename
		self.filename =	zi.filename
		self.date_time = zi.date_time
		self.compress_type = zi.compress_type
		self.comment = zi.comment
		self.extra = zi.extra
		self.create_system = zi.create_system
		self.create_version = zi.create_version
		self.extract_version = zi.extract_version
		self.reserved = zi.reserved
		self.flag_bits = zi.flag_bits
		self.volume = zi.volume
		self.internal_attr = zi.internal_attr
		self.external_attr = zi.external_attr
		self.header_offset = zi.header_offset
		self.CRC = zi.CRC
		self.compress_size = zi.compress_size
		self.file_size = zi.file_size
		self._raw_time = zi._raw_time

class InfoZipFile:
	def __init__(self, filename, mode='r'):
		self.zf = ZipFile(filename, mode)
		self.il = []
		for i in self.zf.infolist():
			self.il += [InfoZipInfo(i)]
	def infolist(self):
		return self.il
	def extractall(self, path=None, members=None, utc_fallback=True):
		if members == None:
			members = self.infolist()
		for i in members:
			self.zf.extract(i, path)
			ut = _zipinfo_unix_time(i)
			if ut == None:
				if not utc_fallback:
					continue
				info(_('Assuming zipinfo time as UTC: %s'), i.filename)
				ut = calendar.timegm(i.date_time)
			os.utime(os.path.join(path, i.filename), (ut, ut))

