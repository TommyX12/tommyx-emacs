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
    return min(max(x, a), b)

def sample(distribution, values):
    choice = random.random() * sum(distribution)
    i, total= 0, distribution[0]
    while choice > total:
        i += 1
        total += distribution[i]
    return values[i]
