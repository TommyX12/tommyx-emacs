
class Completer(object):

	def __init__(self):
		pass

	def complete(self, prefix, context):
		raise NotImplementedError()

class SmartCompleter(Completer):

	def __init__(self):
		Completer.__init__(self)
	
	def complete(self, prefix, context):
		return {
			'candidates': [
				prefix + ' thing1',
				prefix + ' thing2',
				context + ' thing1',
				context + ' thing2',
			]
		}
