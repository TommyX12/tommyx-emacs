import sys
import traceback

def get_traceback():
    '''
    Return error traceback in string.
    '''
    return traceback.format_exc()

def clamp(x, a, b):
    '''
    Clamp x between a and b.
    '''
    return min(max(x, a), b)

