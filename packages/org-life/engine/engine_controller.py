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
            decoded_args = SchedulingRequest().decode(decoded_request.args)
            engine_response = self.engine.schedule(decoded_args)
            return EngineResponse() \
                .set_property('status', 'success') \
                .set_property('data', encoded_response)

        else:
            return EngineResponse() \
                .set_property('status', 'error') \
                .set_property('error', 'unrecognized command')

    def request_handler(self, request):
        try:
            deserialized_request = self.serializer.deserialize(request)
            decoded_request = EngineRequest().decode(deserialized_request)
            response = self.execute(decoded_request)
            encoded_response = response.encode()
            serialized_response = self.serializer.serialize(encoded_response)
            return serialized_response
            
        except Exception:
            err = util.get_traceback()
            self.logger.log(err)

            response = EngineResponse() \
                .set_property('status', 'error') \
                .set_property('error', err)
            
            encoded_response = response.encode()
            serialized_response = self.serializer.serialize(encoded_response)
            return serialized_response

