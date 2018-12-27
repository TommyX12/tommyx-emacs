import sys, traceback, random

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
