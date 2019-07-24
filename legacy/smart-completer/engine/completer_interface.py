import util

class CompleterInterface(object):

	def __init__(self, completer):
		self.completer = completer
		self.commands = {
			'complete': (
				lambda args: self.completer.complete(
					args['prefix'],
					args['context'],
					args['file_name'],
				)
			),
			'parse': (
				lambda args: self.completer.parse(
					args['file_name'],
					args['content'],
				)
			),
			'log_status': (
				lambda args: self.completer.log_status()
			),
		}
		util.print_log('completer started.')

	def request_handler(self, request_data):
		try:
			return self.commands[request_data['command']](request_data['args'])

		except Exception:
			util.print_traceback()
