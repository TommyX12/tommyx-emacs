import re

TOKEN_SEPARATOR_DEFAULT_PATTERN = r'[a-zA-Z0-9_]+|[^a-zA-Z0-9_\s]+'

class TokenSeparator(object):

	def __init__(self, pattern = TOKEN_SEPARATOR_DEFAULT_PATTERN):
		self._compiled_pattern = None
		self.set_pattern(pattern)
		pass

	def set_pattern(self, pattern):
		self._compiled_pattern = re.compile(pattern)

	def get_tokens(self, text):
		"""
		>>> s = TokenSeparator(r'[a-zA-Z0-9_]+|[^a-zA-Z0-9_\s]+')
		>>> s.get_tokens('abc,,,123')
		('abc', ',,,', '123')
		"""
		return tuple(self._compiled_pattern.findall(text))


class NGramParser(object):

	def __init__(self, token_separator, n):
		self.token_separator = token_separator
		self.n = n
		self._tokens = None
		self._i = 0
		self._j = 0

	def parse(self, text):
		self._tokens = self.token_separator.get_tokens(text) # is tuple
		if self._tokens is None:
			return

		if len(self._tokens) == 0:
			self._tokens = None # make self.next() return None

		else:
			n = min(len(self._tokens), self.n)
			self._i = 1 - n
			self._j = 1

	def next(self):
		"""
		>>> parser = NGramParser(TokenSeparator(), 6)
		>>> text = \"\"\"class TokenSeparator(object):\"\"\"
		>>> parser.parse(text)
		>>> parser.next()
		('class',)
		>>> parser.next()
		('class', 'TokenSeparator')
		>>> parser.parse('')
		>>> parser.next()
		>>> parser.parse('1')
		>>> parser.next()
		('1',)
		>>> parser.next()
		"""
		if self._tokens is None:
			return None

		if self._i >= len(self._tokens):
			return None

		result = self._tokens[
			max(0, self._i):
			min(len(self._tokens), self._j)
		]

		self._i += 1
		self._j += 1

		return result


if __name__ == '__main__':
	import doctest
	doctest.testmod()

	parser = NGramParser(TokenSeparator(), 6)
	text = """
		class TokenSeparator(object):
			def __init__(self, a):
				pass
	"""

	parser.parse(text)
	while True:
		ngram = parser.next()

		if ngram is None:
			break

		print(ngram)
