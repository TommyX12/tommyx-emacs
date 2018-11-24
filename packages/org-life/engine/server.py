import util

from logger import DummyLogger

class Server(object):

    def __init__(self, handler):
        self.handler = handler

    def listen(self, args):
        raise NotImplementedError()

class STDIOServer(Server):

    def __init__(self, handler, logger = DummyLogger()):
        Server.__init__(self, handler)

        self.logger = logger

    def listen(self, args = None):
        self.logger.log('server started.')
        while True:
            request = str(input())
            response = self.handler(request)
            print(response)
