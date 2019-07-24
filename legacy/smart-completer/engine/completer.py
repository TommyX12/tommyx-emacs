if __name__ == '__main__':
	import sys
	sys.path.append('..')
	from container import CompletionCache
	from text_parser import NGramParser, TokenSeparator

else:
	from .container import CompletionCache
	from .text_parser import NGramParser, TokenSeparator

import util

import json

CONTEXT_LENGTH = 6
NUM_CANDIDATES = 10

class Completer(object):

	def __init__(self):
		pass

	def complete(self, prefix, context):
		raise NotImplementedError()

class SmartCompleter(Completer):

	def __init__(
			self,
			context_length = CONTEXT_LENGTH,
			num_candidates = NUM_CANDIDATES
		):
		Completer.__init__(self)
		self.context_length = context_length
		self.num_candidates = num_candidates
		self.token_separator = TokenSeparator()
		self.ngram_parser = NGramParser(self.token_separator, context_length)
		self.caches = {}
	
	def complete(self, prefix, context, file_name):
		"""
		>>> completer = SmartCompleter(num_candidates = 99999)
		>>> completer.parse("a.txt", "what is going on here, going on how")
		>>> completer.parse("b.txt", "what is going on what, going on why")
		>>> sorted(completer.complete("h", "going on", "a.txt")['candidates'])
		['here', 'how']
		>>> sorted(completer.complete("", "going on", "a.txt")['candidates'])
		[',', 'going', 'here', 'how', 'is', 'on', 'what']
		>>> sorted(completer.complete("", "", "a.txt")['candidates'])
		[',', 'going', 'here', 'how', 'is', 'on', 'what']
		>>> sorted(completer.complete("h", "going on", "b.txt")['candidates'])
		[]
		>>> sorted(completer.complete("", "going on", "b.txt")['candidates'])
		[',', 'going', 'is', 'on', 'what', 'why']
		>>> sorted(completer.complete("", "", "b.txt")['candidates'])
		[',', 'going', 'is', 'on', 'what', 'why']
		>>> completer.complete("", "", "c.txt")
		{'not_parsed': True}
		"""
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

		candidates = []
		candidates_set = {}
		for i in range(len(context_ngram) + 1):
			completions = cache.complete(
				prefix, context_ngram[i:],
				self.num_candidates
			)
			for completion in completions:
				if completion not in candidates_set:
					candidates.append(completion)
					candidates_set[completion] = True

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

	def log_status(self):
		status = {
			"num_files_cached": len(self.caches)
		}
		util.print_log(json.dumps(status, sort_keys = True, indent = 4))


if __name__ == '__main__':
	print("running doctest.")
	import doctest
	doctest.testmod()

	print("running memory test.")
	import os
	import psutil
	def get_memory_usage():
		process = psutil.Process(os.getpid())
		return process.memory_info().rss

	import random, string
	random.seed(0)
	word_size = 6
	vocab_size = 500
	corpus_size = 10000
	chars = string.ascii_uppercase + string.digits
	vocab = [''.join(random.choice(chars) for _ in range(word_size)) for _ in range(vocab_size)]
	corpus = ' '.join(random.choice(vocab) for _ in range(corpus_size))
	memory_before = get_memory_usage()
	completer = SmartCompleter()
	completer.parse("a.txt", corpus)
	memory_after = get_memory_usage()
	print("cache memory:", memory_after - memory_before)
	print("corpus memory:", sys.getsizeof(corpus))
	print("cache memory per corpus byte:", (memory_after - memory_before) / sys.getsizeof(corpus))

