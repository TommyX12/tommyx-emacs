import copy

from datetime import date as DatetimeDate, timedelta
from enum import Enum


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
        Property.__init__(self)
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


class Command(PrimitiveProtocol):
    def __init__(self, value = None):
        PrimitiveProperty.__init__(self, value)

class Duration(PrimitiveProtocol):
    def __init__(self, value = 0):
        PrimitiveProperty.__init__(self, value)

class Ratio(PrimitiveProtocol):
    def __init__(self, value = 0.0):
        PrimitiveProperty.__init__(self, value)

class Days(PrimitiveProtocol):
    def __init__(self, value = 0):
        PrimitiveProperty.__init__(self, value)

class TaskID(PrimitiveProtocol):
    def __init__(self, value = 0):
        PrimitiveProperty.__init__(self, value)

class Priority(PrimitiveProtocol):
    def __init__(self, value = 0):
        PrimitiveProperty.__init__(self, value)

class Date(Protocol):

    def __init__(self, date_object = None):
        Protocol.__init__(self)
        if date_object is None:
            date_object = DatetimeDate.today()

        self._date = date_object
        
    def encode(self):
        return '-'.join(self._date.year, self._date.month, self._date.day)

    def from_components(year, month, day):
        return Date(DatetimeDate(year, month, day))

    def decode(self, encoded_protocol):
        components = [int(component) for component in encoded_protocol.split('-')]
        self._date = DatetimeDate(*components)

    def today():
        return Date(DatetimeDate.today())

    def get_weekday():
        '''
        Monday is 0 and Sunday is 6.
        '''
        return self._date.weekday()

    def copy(self):
        return Date(self._date)

    def add_days(self, days):
        return Date(self._date + timedelta(days = days))

    def __eq__(self, date):
        return self._date == date._date

    def __lt__(self, date):
        return self._date < date._date

    def __hash__(self):
        '''
        Make possible to be used as dictionary key.
        '''
        return hash(self._date)

class Config(Protocol):
    properties = {
        'today': ObjectProperty(Date),
        'scheduling_days': ObjectProperty(Days),
        'daily_info_days': ObjectProperty(Days),
        'fragment_max_stress': ObjectProperty(Ratio),
        'fragment_max_percentage': ObjectProperty(Ratio),
    }

class Task(Protocol):
    properties = {
        'id': ObjectProperty(TaskID),
        'from': ObjectProperty(Date),
        'to': ObjectProperty(Date),
        'amount': ObjectProperty(Duration),
        'done': ObjectProperty(Duration),
        'priority': ObjectProperty(Priority),
    }

class WorkTimeSelector(PrimitiveProtocol):
    def __init__(self, value = 'default'):
        PrimitiveProtocol.__init__(self, value)
        
    def encode(self):
        return self.value

    def decode(self, encoded_protocol):
        self.value = encoded_protocol.lower()

class WorkTimeConfigEntry(Protocol):
    properties = {
        'selector': ObjectProperty(WorkTimeSelector),
        'duration': ObjectProperty(Duration),
    }

class SchedulingRequest(Protocol):
    properties = {
        'config': ObjectProperty(Config),
        'tasks': ListProperty(Task),
        'work_time': ListProperty(WorkTimeConfigEntry),
    }

class SchedulingGeneralInfo(Protocol):
    properties = {
        "stress": ObjectProperty(Ratio),
    }

class ImpossibleTask(Protocol):
    properties = {
        "id": ObjectProperty(TaskID),
    }

class Alerts(Protocol):
    properties = {
        "impossible_tasks": ListProperty(ImpossibleTask),
    }

class SessionAmount(Protocol):
    properties = {
        "current": ObjectProperty(Duration),
        "total": ObjectProperty(Duration),
        "stress": ObjectProperty(Ratio),
    }

class SessionTypeEnum(Enum):
    TASK = 0
    FRAGMENT = 1

class SessionType(PrimitiveProtocol):
    def __init__(self, value = SessionTypeEnum.TASK):
        PrimitiveProperty.__init__(self, value)

    def encode(self):
        return self.value.value

    def decode(self, encoded_protocol):
        self.value = SessionTypeEnum(encoded_protocol)

class Session(Protocol):
    properties = {
        "id": ObjectProperty(TaskID),
        "amount": ObjectProperty(SessionAmount),
        "type": ObjectProperty(SessionType),
    }

    def __init__(self):
        Protocol.__init__(self)
        raise NotImplementedError()

class DailyFragmentAmount(Protocol):
    properties = {
        "current": ObjectProperty(Duration),
        "total": ObjectProperty(Duration),
    }

class DailyFragmentEntry(Protocol):
    properties = {
        "id": ObjectProperty(TaskID),
        "amount": ObjectProperty(DailyFragmentAmount),
    }

class DailyInfo(Protocol):
    properties = {
        "date": ObjectProperty(Date),
        "work_time": ObjectProperty(Duration),
        "sessions": ListProperty(Session),
        "free_time": ObjectProperty(Duration),
        "average_stress": ObjectProperty(Ratio),
    }

class SchedulingResponse(Protocol):
    properties = {
        "general": ObjectProperty(SchedulingGeneralInfo),
        "alerts": ObjectProperty(Alerts),
        "daily_infos": ListProperty(DailyInfo),
    }

class EngineRequest(Protocol):
    properties = {
        'command': PrimitiveProperty(),
        'args': PrimitiveProperty(),
    }

class EngineResponse(Protocol):
    properties = {
        'status': PrimitiveProperty(),
        'error': PrimitiveProperty(),
        'data': PrimitiveProperty(),
    }

class FreeTimeInfo(Protocol):
    properties = {
        "free_time": ObjectProperty(Duration),
        "average_stress": ObjectProperty(Ratio),
    }
