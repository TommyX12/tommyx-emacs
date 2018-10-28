
class Viewable(object):

	__slots__ = ['iterable']

	def __init__(self, iterable):
		self.iterable = iterable
		self.view = View(self, 0, len(self.iterable))

	def __getitem__(self, range):
		return self.get_view(range)

	def get_view(self, range):
		return View(self, range[0], range[1])

	def range(self, start, end):
		"""
		Make a range on the current viewable object in the range [start, end).
		Call get_view to access the view using the range.
		"""
		return (start, end)

class View(object):

	__slots__ = ['viewable', 'start', 'end']

	def __init__(self, viewable, start, end):
		self.viewable
		self.start = start
		self.end = end

	def __getitem__(self, i):
		if isinstance(k, slice):
			# only simple slices are supported
			return View(
				self.viewable,
				self.start + slice.start,
				self.start + slice.end
			)

		return self.viewable.iterable[start + i]

	def __len__(self):
		return self.end - self.start
