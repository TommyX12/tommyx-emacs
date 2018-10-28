
class Counter(object):

	__slots__ = ['count']

	def __init__(self, count = 0):
		self.count = count


class CacheCharNode(object):

	__slots__ = ['char', 'next_chars', 'token_nodes', 'exact_token_node']

	def __init__(self, char):
		self.char = char
		self.next_chars = {}
		self.token_nodes = []
		self.exact_token_node = None

	def add_token(self, token, next_token_node):
		if len(token) == 0:
			self.token_nodes.append(next_token_node)
			self.exact_token_node = next_token_node
			# TODO will change for other type of expansions.
			return

		char = token[0]
		rest = token[1:]

		if char not in self.next_chars:
			self.next_chars[char] = CacheCharNode(char)

		self.next_chars[char].add_token(rest, next_token_node)

	def __contains__(self, token):
		return self._contains_dfs(token, token)

	def _contains_dfs(self, token_old, token):
		if len(token) == 0:
			return self.exact_token_node is not None and \
				self.exact_token_node.token == token_old

		else:
			char = token[0]
			rest = token[1:]
			if char not in self.next_chars:
				return

			return self.next_chars[char]._contains_dfs(token_old, rest)

	def __getitem__(self, token):
		if len(token) == 0:
			return self.exact_token_node

		else:
			char = token[0]
			rest = token[1:]
			if char not in self.next_chars:
				return

			return self.next_chars[char][rest]

	def complete(self, prefix, counter, results):
		if counter.count <= 0:
			return

		if len(prefix) == 0:
			for token_node in self.token_nodes:
				if counter.count > 0:
					results.append(token_node.token)
					counter.count -= 1

				else:
					break

			for next_char in self.next_chars:
				if counter.count > 0:
					self.next_chars[next_char].complete(prefix, counter, results)

				else:
					break

		else:
			char = prefix[0]
			rest = prefix[1:]
			if char not in self.next_chars:
				return

			self.next_chars[char].complete(rest, counter, results)


class CacheTokenNode(object):

	__slots__ = ['token', 'char_root']

	def __init__(self, token):
		self.token = token
		self.char_root = CacheCharNode(None)
		# self.next_tokens = {}

	def add_ngram(self, ngram):
		# ngram is a tuple
		if len(ngram) == 0:
			return

		token = ngram[0]
		rest = ngram[1:]

		if token not in self.char_root:
			next_token_node = CacheTokenNode(token)
			self.char_root.add_token(token, next_token_node)

		self.char_root[token].add_ngram(rest)

	def complete(self, prefix, context_ngram, count):
		results = []
		self._dfs(prefix, context_ngram, Counter(count), results)
		return results

	def _dfs(self, prefix, context_ngram, counter, results):
		if counter.count <= 0:
			return

		if len(context_ngram) == 0:
			self.char_root.complete(prefix, counter, results)

		else:
			token = context_ngram[0]
			rest = context_ngram[1:]
			if token not in self.char_root:
				return

			self.char_root[token]._dfs(prefix, rest, counter, results)


class CompletionCache(object):

	__slots__ = ['root']

	def __init__(self):
		self.root = CacheTokenNode(None)

	def add_ngram(self, ngram):
		self.root.add_ngram(ngram)

	def complete(self, prefix, context_ngram, count):
		return self.root.complete(prefix, context_ngram, count)

	def clear(self):
		self.root = CacheTokenNode(None)


if __name__ == '__main__':
	import doctest
	doctest.testmod()

	from text_parser import NGramParser, TokenSeparator
	parser = NGramParser(TokenSeparator(), 6)
	text = """
	class CompletionCache(object):
		def __init__(self):
			self.root = CacheTokenNode(None)
			self.abc = 0
		def add_ngram(self, ngram):
			self.root.add_ngram(ngram)
		def add_ngram_2(self, ngram):
			self.root.add_ngram(ngram)
		def complete(self, prefix, context_ngram, count):
			return self.root.complete(prefix, context_ngram, count)
	"""

	parser.parse(text)

	cache = CompletionCache()
	while True:
		ngram = parser.next()

		if ngram is None:
			break

		cache.add_ngram(ngram)

	print(cache.complete("a", ("def",), 10))
