#!/usr/bin/env python

import sys

for fn in sys.argv[1:]:
	try:
		f = open(fn)
	except FileNnotFoundError:
		print("{}という名前のファイルは存在しません".format(fn))
	else:
		try:
			print(fn, len(f.read()))
		finally:
			f.close()
