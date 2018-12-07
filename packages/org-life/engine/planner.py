
from data_structure import *


class Planner(object):

    def __init__(self):
        pass

    def plan(self, tasks, schedule, direction = FillDirection.EARLY):
        raise NotImplementedError()


def main():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    main()

