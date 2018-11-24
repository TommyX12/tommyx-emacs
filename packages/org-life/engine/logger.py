import sys


class Logger(object):

    def __init__(self):
        pass

    def log(self, message):
        raise NotImplementedError()


class STDERRLogger(object):

    def __init__(self):
        pass

    def log(self, message):
        print(message, file = sys.stderr)


class DummyLogger(object):

    def __init__(self):
        pass

    def log(self, message):
        pass
