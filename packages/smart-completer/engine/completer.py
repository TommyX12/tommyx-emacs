
class Completer(object):

	def __init__(self):
		pass

	def request_handler(self, request_data):
		prefix = request_data['prefix']
		results = {
			'candidates': [
				(prefix + "what"),
				(prefix + "wht"),
				(prefix + "interactive"),
				(prefix + "inwg"),
				(prefix + "interavve"),
				(prefix + "interaction")
			],
		}
		return results
