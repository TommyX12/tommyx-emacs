
class CacheCharNode(object):

	def __init__(self, char):
		self.char = char
		self.next_chars = {}
		self.token_nodes = []

	def add_token(self, token, next_token_node):
		if len(token) == 0:
			self.token_nodes.append(next_token_node)

		char = token[0]
		rest = token[1:]

		if char not in self.next_chars:
			self.next_chars[char] = CacheCharNode(char)

		self.next_chars[char].add_token(rest, next_token_node)


class CacheTokenNode(object):

	def __init__(self, token):
		self.token = token
		self.char_root = CacheCharNode(None)

	def add_ngram(self, ngram):
		pass

class CompletionCache(object):

	def __init__(self):
		self.root = CacheTokenNode(None)
		pass

	def add_ngram(self, ngram):
		self.root.add_ngram(ngram)
