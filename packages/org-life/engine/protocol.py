import copy

from datetime import date

class Property(object):

    def __init__(self):
        pass

    def get_default_value(self):
        raise NotImplementedError()

    def encode(self, data):
        raise NotImplementedError()

    def decode(self, encoded):
        raise NotImplementedError()

class PrimitiveProperty(Property):
    
    def __init__(self, default_value = None):
        Property.__init__(self)
        self.default_value = default_value
        
    def get_default_value(self):
        return copy.deepcopy(self.default_value)

    def encode(self, data):
        return data

    def decode(self, encoded):
        return encoded

class ObjectProperty(Property):

    def __init__(self, prop_type):
        Property.__init__(self)
        self.prop_type = prop_type

    def get_default_value(self):
        return self.prop_type()

    def encode(self, prop):
        return prop.encode() if prop is not None else None

    def decode(self, encoded_prop):
        return self.prop_type().decode(encoded_prop) if encoded_prop is not None else None

class ListProperty(Property):

    def __init__(self, prop_type):
        Property.__init__(self)
        self.prop_type = prop_type

    def get_default_value(self):
        return []

    def encode(self, props):
        return [
            prop.encode()
            for prop in props
        ] if props is not None else None

    def decode(self, encoded_props):
        return [
            self.prop_type().decode(encoded_prop)
            for encoded_prop in encoded_props
        ] if encoded_props is not None else None

class DictProperty(Property):
    
    def __init__(self, prop_type):
        Property.__init__(self, default_value)
        self.prop_type = prop_type

    def get_default_value(self):
        return {}

    def encode(self, props):
        return {
            prop_name: props[prop_name].encode()
            for prop_name in props
        } if props is not None else None

    def decode(self, encoded_props):
        return {
            prop_name: self.prop_type().decode(encoded_props[prop_name])
            for prop_name in encoded_props
        } if encoded_props is not None else None

    
class Protocol(object):

    properties = {}

    def __init__(self):
        props = type(self).properties
        for prop_name in props:
            prop = props[prop_name]
            self.set_property(prop_name, prop.get_default_value())

    def encode(self):
        result = {}
        props = type(self).properties
        for prop_name in props:
            prop = props[prop_name]
            result[prop_name] = prop.encode(self.get_property(prop_name))

        return result

    def decode(self, encoded_protocol):
        props = type(self).properties
        for prop_name in props:
            if prop_name not in encoded_protocol:
                continue

            prop = props[prop_name]
            self.set_property(prop_name, prop.decode(encoded_protocol[prop_name]))

        return self # allows chaining

    def get_property(self, prop_name):
        return self.__dict__[prop_name]

    def set_property(self, prop_name, value):
        self.__dict__[prop_name] = value
        
        return self # allows chaining
    

class PrimitiveProtocol(Protocol):
    
    properties = {}

    def __init__(self, default_value = None):
        Protocol.__init__(self)
        self.value = default_value

    def encode(self):
        return self.value

    def decode(self, encoded_protocol):
        self.value = encoded_protocol
    

# ============= Implementations =============


class CommandProtocol(PrimitiveProtocol):
    def __init__(self):
        PrimitiveProperty.__init__(self, None)

class DurationProtocol(PrimitiveProtocol):
    def __init__(self):
        PrimitiveProperty.__init__(self, 0)

class RatioProtocol(PrimitiveProtocol):
    def __init__(self):
        PrimitiveProperty.__init__(self, 0.0)

class DaysProtocol(PrimitiveProtocol):
    def __init__(self):
        PrimitiveProperty.__init__(self, 0)

class IDProtocol(PrimitiveProtocol):
    def __init__(self):
        PrimitiveProperty.__init__(self, 0)

class PriorityProtocol(PrimitiveProtocol):
    def __init__(self):
        PrimitiveProperty.__init__(self, 0)

class DateProtocol(PrimitiveProtocol):
    def __init__(self):
        PrimitiveProtocol.__init__(self, date.today())
        
    def encode(self):
        return '-'.join(self.value.year, self.value.month, self.value.day)

    def decode(self, encoded_protocol):
        components = [int(component) for component in encoded_protocol.split('-')]
        self.value = date(*components)

class ConfigProtocol(Protocol):
    properties = {
        'today': ObjectProperty(DateProtocol),
        'scheduling_days': ObjectProperty(DaysProtocol),
        'daily_info_days': ObjectProperty(DaysProtocol),
        'fragment_max_stress': ObjectProperty(RatioProtocol),
        'fragment_max_percentage': ObjectProperty(RatioProtocol),
    }

class TaskProtocol(Protocol):
    properties = {
        'id': ObjectProperty(IDProtocol),
        'from': ObjectProperty(DateProtocol),
        'to': ObjectProperty(DateProtocol),
        'amount': ObjectProperty(DurationProtocol),
        'done': ObjectProperty(DurationProtocol),
        'priority': ObjectProperty(PriorityProtocol),
    }

class SchedulingRequestProtocol(Protocol):
    properties = {
        'config': ObjectProperty(ConfigProtocol),
        'tasks': ListProperty(TaskProtocol),
        'work_time': DictProperty(DurationProtocol)
    }

class SchedulingGeneralInfoProtocol(Protocol):
    properties = {
        "stress": ObjectProperty(RatioProtocol),
    }

class ImpossibleTaskProtocol(Protocol):
    properties = {
        "id": ObjectProperty(IDProtocol),
    }

class AlertsProtocol(Protocol):
    properties = {
        "impossible": ListProperty(ImpossibleTaskProtocol),
    }

class DailyScheduleAmountProtocol(Protocol):
    properties = {
        "current": ObjectProperty(DurationProtocol),
        "total": ObjectProperty(DurationProtocol),
        "stress": ObjectProperty(RatioProtocol),
    }

class DailyScheduleEntryProtocol(Protocol):
    properties = {
        "id": ObjectProperty(IDProtocol),
        "amount": ObjectProperty(DailyScheduleAmountProtocol),
    }

class DailyFragmentAmountProtocol(Protocol):
    properties = {
        "current": ObjectProperty(DurationProtocol),
        "total": ObjectProperty(DurationProtocol),
    }

class DailyFragmentEntryProtocol(Protocol):
    properties = {
        "id": ObjectProperty(IDProtocol),
        "amount": ObjectProperty(DailyFragmentAmountProtocol),
    }

class DailyInfoProtocol(Protocol):
    properties = {
        "date": ObjectProperty(DateProtocol),
        "work_time": ObjectProperty(DurationProtocol),
        "schedule": ListProperty(DailyScheduleEntryProtocol),
        "fragments": ListProperty(DailyFragmentEntryProtocol),
        "free_time": ObjectProperty(DurationProtocol),
        "average_stress": ObjectProperty(RatioProtocol),
    }

class SchedulingResponseProtocol(Protocol):
    properties = {
        "general": ObjectProperty(SchedulingGeneralInfoProtocol),
        "alerts": ObjectProperty(AlertsProtocol),
        "daily_info": ListProperty(DailyInfoProtocol),
    }

class EngineRequestProtocol(Protocol):
    properties = {
        'command': PrimitiveProperty(),
        'args': PrimitiveProperty(),
    }

class EngineResponseProtocol(Protocol):
    properties = {
        'status': PrimitiveProperty(),
        'error': PrimitiveProperty(),
        'data': PrimitiveProperty(),
    }
