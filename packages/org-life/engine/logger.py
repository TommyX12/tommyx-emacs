import sys


class Logger(object):

    def __init__(self):
        raise NotImplementedError()

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


class InternalLogger(Logger):

    def __init__(self):
        self.messages = []

    def log(self, message):
        self.messages.append(message)

    def clear(self):
        self.messages = []

    def get_messages(self):
        return self.messages
