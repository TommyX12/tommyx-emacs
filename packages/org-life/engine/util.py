import sys, traceback, random, time

def get_traceback():
    '''
    Return error traceback in string.
    '''
    return traceback.format_exc()

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
