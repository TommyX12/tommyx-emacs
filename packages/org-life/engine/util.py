import sys
import traceback
import random
import time


def get_traceback():
    '''
    Return error traceback in string.
    '''
    return traceback.format_exc()


def less_than(a, b):
    return a < b


def lower_bound(arr, first, last, val, comp=None):
    '''
    Return the index, in the slice of "arr" denoted by ["first", "last"),
    pointing to the first element that is greater than or equal to "val",
    using "comp" as a less-than operator.
    '''
    if comp is None:
        comp = less_than

    count = last - first

    while count > 0:
        it = first
        step = count // 2
        it += step

        if comp(arr[it], val):
            it += 1
            first = it
            count -= step + 1

        else:
            count = step

    return first


def linear_map(x, in_min, in_max, out_min, out_max):
    return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min


def clamp(x, a, b):
    '''
    Clamp x between a and b.
    '''
    if a is None:
        if b is None:
            return x

        return min(x, b)

    if b is None:
        return max(x, a)

    return min(max(x, a), b)


class PerformanceTimer(object):

    def __init__(self):
        self.time_stack = []

    def push(self):
        self.time_stack.append(time.time())

    def pop(self):
        time_before = self.time_stack.pop()
        time_now = time.time()
        return time_now - time_before
