import json
import util

class Protocol(object):

	def __init__(self):
		pass

	def encode(self, data):
		raise NotImplementedError()

	def decode(self, message):
		raise NotImplementedError()


class JSONProtocol(Protocol):

	def __init__(self):
		Protocol.__init__(self)

	def encode(self, data):
		try:
			return json.dumps(data)

		except Exception:
			util.print_traceback()
			return None

	def decode(self, message):
		try:
			return json.loads(message)

		except Exception:
			util.print_traceback()
			return "null"


