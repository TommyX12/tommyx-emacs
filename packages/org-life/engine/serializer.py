import json

class Serializer(object):

    def __init__(self):
        pass

    def serialize(self, encoded_protocol):
        raise NotImplementedError()

    def deserialize(self, serialized_protocol):
        raise NotImplementedError()


class JSONSerializer(object):

    def serialize(self, encoded_protocol):
        return json.dumps(encoded_protocol)

    def deserialize(self, serialized_protocol):
        return json.loads(serialized_protocol)
