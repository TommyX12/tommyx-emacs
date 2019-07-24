from bisect import insort, bisect_left

class Counter(object):

	__slots__ = ['count']

	def __init__(self, count = 0):
		self.count = count


class TokenCache(object):

	def __init__(self):
		pass

	def add_token(self, token, next_token_node):
		raise NotImplementedError()

	def __contains__(self, token):
		raise NotImplementedError()

	def __getitem__(self, token):
		raise NotImplementedError()

	def complete(self, prefix, counter, results):
		raise NotImplementedError()


class NGramCache(object):

	def __init__(self):
		pass

	def add_ngram(self, ngram):
		raise NotImplementedError()

	def complete(self, prefix, context_ngram, count):
		raise NotImplementedError()


class SortedArrayTokenCache(object):

	__slots__ = ['next_token_nodes']

	def __init__(self):
		self.next_token_nodes = []

	def add_token(self, token, next_token_node):
		insort(self.next_token_nodes, (token, next_token_node))

	def __contains__(self, token):
		index = bisect_left(self.next_token_nodes, (token,))
		return (
			index >= 0 and index < len(self.next_token_nodes)
			and self.next_token_nodes[index][0] == token
		)

	def __getitem__(self, token):
		index = bisect_left(self.next_token_nodes, (token,))
		# TODO have to change here when dealing with multiple expansions
		if index >= 0 and index < len(self.next_token_nodes):
			return self.next_token_nodes[index][1]

	def complete(self, prefix, counter, results):
		index = bisect_left(self.next_token_nodes, (prefix,))
		while index < len(self.next_token_nodes) and counter.count > 0:
			if self.next_token_nodes[index][0].startswith(prefix):
				results.append(self.next_token_nodes[index][1].token)
				counter.count -= 1
				index += 1

			else:
				break


class TrieTokenCache(object):

	__slots__ = ['char', 'next_chars', 'token_nodes', 'exact_token_node']

	def __init__(self, char = None):
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
			self.next_chars[char] = TrieTokenCache(char)

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

class TrieNGramCache(object):

	__slots__ = ['token', 'char_cache']

	def __init__(self, token = None):
		self.token = token
		# self.char_cache = TrieTokenCache()
		self.char_cache = SortedArrayTokenCache()

	def add_ngram(self, ngram):
		# ngram is a tuple
		if len(ngram) == 0:
			return

		token = ngram[0]
		rest = ngram[1:]

		if token not in self.char_cache:
			next_token_node = TrieNGramCache(token)
			self.char_cache.add_token(token, next_token_node)

		self.char_cache[token].add_ngram(rest)

	def complete(self, prefix, context_ngram, count):
		results = []
		self._dfs(prefix, context_ngram, Counter(count), results)
		return results

	def _dfs(self, prefix, context_ngram, counter, results):
		if counter.count <= 0:
			return

		if len(context_ngram) == 0:
			self.char_cache.complete(prefix, counter, results)

		else:
			token = context_ngram[0]
			rest = context_ngram[1:]
			if token not in self.char_cache:
				return

			self.char_cache[token]._dfs(prefix, rest, counter, results)


class CompletionCache(object):

	__slots__ = ['root']

	def __init__(self):
		self.root = TrieNGramCache()

	def add_ngram(self, ngram):
		self.root.add_ngram(ngram)

	def complete(self, prefix, context_ngram, count):
		return self.root.complete(prefix, context_ngram, count)

	def clear(self):
		self.root = TrieNGramCache()


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
