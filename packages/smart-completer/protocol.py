import json

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
		return json.dumps(data)

	def decode(self, message):
		return json.loads(message)


