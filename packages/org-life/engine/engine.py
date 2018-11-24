import util

from logger import DummyLogger

class Engine(object):

    commands = {
        'complete': (
            lambda self, args: self.completer.complete(
                args['prefix'],
                args['context'],
                args['file_name'],
            )
        ),
        'parse': (
            lambda self, args: self.completer.parse(
                args['file_name'],
                args['content'],
            )
        ),
        'log_status': (
            lambda self, args: self.completer.log_status()
        ),
    }
    
    def __init__(self, logger = DummyLogger()):
        self.logger = logger

