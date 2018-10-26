if __name__ == '__main__':
	import sys
	sys.path.append('..')

import util
from .container import CompletionCache
from .text_parser import NGramParser, TokenSeparator

CONTEXT_LENGTH = 6
NUM_CANDIDATES = 10

class Completer(object):

	def __init__(self):
		pass

	def complete(self, prefix, context):
		raise NotImplementedError()

class SmartCompleter(Completer):

	def __init__(self):
		Completer.__init__(self)
		self.token_separator = TokenSeparator()
		self.ngram_parser = NGramParser(self.token_separator, CONTEXT_LENGTH)
		self.caches = {}

		self.caches
	
	def complete(self, prefix, context, file_name):
		if file_name not in self.caches:
			return {'not_parsed': True}

		cache = self.caches[file_name]

		context_ngram = self.token_separator.get_tokens(context)

		# # this maybe cause problem, so commented out for now
		# prefix_ngram = self.token_separator.get_tokens(prefix)
		# if len(prefix_ngram) > 1:
		# 	prefix = prefix_ngram[-1]
		# 	context_ngram += prefix_ngram[:-1]

		if len(context_ngram) > CONTEXT_LENGTH - 1:
			context_ngram = context_ngram[-(CONTEXT_LENGTH - 1):]

		count = NUM_CANDIDATES
		candidates = []
		candidates_set = {}
		for i in range(len(context_ngram) + 1):
			if count <= 0:
				break

			completions = cache.complete(prefix, context_ngram[i:], count)
			for completion in completions:
				if completion not in candidates_set:
					candidates.append(completion)
					candidates_set[completion] = True

			count -= len(completions)

		return {'candidates': candidates}

	def parse(self, file_name, content):
		if file_name is None:
			return

		if file_name not in self.caches:
			self.caches[file_name] = CompletionCache()

		cache = self.caches[file_name]
		cache.clear()

		self.ngram_parser.parse(content)
		while True:
			ngram = self.ngram_parser.next()

			if ngram is None:
				break

			cache.add_ngram(ngram)


if __name__ == '__main__':
	import doctest
	doctest.testmod()

	completer = SmartCompleter()

	completer.parse("a.txt", "what is going on here, going on how")
	completer.parse("b.txt", "what is going on what, going on why")

	print(completer.complete("h", "going on", "a.txt"))
	print(completer.complete("", "going on", "a.txt"))
	print(completer.complete("", "", "a.txt"))
	print(completer.complete("h", "going on", "b.txt"))
	print(completer.complete("", "going on", "b.txt"))
	print(completer.complete("", "", "b.txt"))
