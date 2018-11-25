import json
from datetime import date

import util
from logger import DummyLogger
from protocol import *

class EngineController(object):

    def __init__(self, engine, serializer, logger = DummyLogger()):
        self.engine = engine
        self.serializer = serializer
        self.logger = logger

    def execute(self, decoded_request):
        command = decoded_request.command
        if command == 'schedule':
            decoded_args = SchedulingRequestProtocol().decode(decoded_request.args)
            engine_response = self.engine.schedule(decoded_args)
            return EngineResponseProtocol() \
                .set_property('status', 'success') \
                .set_property('data', encoded_response)

        else:
            return EngineResponseProtocol() \
                .set_property('status', 'error') \
                .set_property('error', 'unrecognized command')

    def request_handler(self, request):
        try:
            deserialized_request = self.serializer.deserialize(request)
            decoded_request = EngineRequestProtocol().decode(deserialized_request)
            response = self.execute(decoded_request)
            encoded_response = response.encode()
            serialized_response = self.serializer.serialize(encoded_response)
            return serialized_response
            
        except Exception:
            err = util.get_traceback()
            self.logger.log(err)

            response = EngineResponseProtocol() \
                .set_property('status', 'error') \
                .set_property('error', err)
            
            encoded_response = response.encode()
            serialized_response = self.serializer.serialize(encoded_response)
            return serialized_response

