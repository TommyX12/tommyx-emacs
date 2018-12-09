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
        return self.prop_type().decode_self(encoded_prop) if encoded_prop is not None else None

    
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
            self.prop_type().decode_self(encoded_prop)
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
            prop_name: self.prop_type().decode_self(encoded_props[prop_name])
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

    def decode_self(self, encoded_protocol):
        '''
        Decode and return self.
        Allows chaining.
        '''

        self.decode(encoded_protocol)
        return self

    def decode(self, encoded_protocol):
        props = type(self).properties
        for prop_name in props:
            if prop_name not in encoded_protocol:
                continue

            prop = props[prop_name]
            self.set_property(prop_name, prop.decode(encoded_protocol[prop_name]))

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
        PrimitiveProtocol.__init__(self, value)

class Duration(PrimitiveProtocol):
    def __init__(self, value = 0):
        PrimitiveProtocol.__init__(self, value)

class Ratio(PrimitiveProtocol):
    def __init__(self, value = 0.0):
        PrimitiveProtocol.__init__(self, value)

class Days(PrimitiveProtocol):
    def __init__(self, value = 0):
        PrimitiveProtocol.__init__(self, value)

class TaskID(PrimitiveProtocol):
    def __init__(self, value = 0):
        PrimitiveProtocol.__init__(self, value)

class Priority(PrimitiveProtocol):
    def __init__(self, value = 0):
        PrimitiveProtocol.__init__(self, value)

class Date(Protocol):

    def __init__(self, date_object = None):
        Protocol.__init__(self)
        if date_object is None:
            date_object = DatetimeDate.today()

        self._date = date_object
        
    def encode(self):
        return '-'.join([str(x) for x in [self._date.year, self._date.month, self._date.day]])

    def from_components(year, month, day):
        return Date(DatetimeDate(year, month, day))

    def decode(self, encoded_protocol):
        if encoded_protocol == 'min':
            self._date = DatetimeDate.min

        elif encoded_protocol == 'max':
            self._date = DatetimeDate.max

        elif encoded_protocol == 'today':
            self._date = DatetimeDate.today()

        else:
            components = [int(component) for component in encoded_protocol.split('-')]
            self._date = DatetimeDate(*components)

    def today():
        return Date(DatetimeDate.today())

    def get_weekday(self):
        '''
        Monday is 0 and Sunday is 6.
        '''
        return self._date.weekday()

    def copy(self):
        return Date(self._date)

    def days_to(self, date):
        return (date._date - self._date).days

    def add_days(self, days):
        return Date(self._date + timedelta(days = days))

    def __eq__(self, date):
        return self._date == date._date

    def __lt__(self, date):
        return self._date < date._date

    def __le__(self, date):
        return self._date <= date._date

    def __hash__(self):
        '''
        Make possible to be used as dictionary key.
        '''
        return hash(self._date)

class FragmentationConfig(Protocol):
    properties = {
        'max_stress': ObjectProperty(Ratio),
        'max_percentage': ObjectProperty(Ratio),
        'fragment_size': ObjectProperty(Duration),
    }

class Config(Protocol):
    properties = {
        'today': ObjectProperty(Date),
        'scheduling_days': ObjectProperty(Days),
        'daily_info_days': ObjectProperty(Days),
        'fragmentation_config': ObjectProperty(FragmentationConfig),
    }

class Task(Protocol):
    properties = {
        'id': ObjectProperty(TaskID),
        'start': ObjectProperty(Date),
        'end': ObjectProperty(Date),
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
        'stress': ObjectProperty(Ratio),
    }

class ImpossibleTask(Protocol):
    properties = {
        'id': ObjectProperty(TaskID),
        'amount': ObjectProperty(Duration),
    }

class Alerts(Protocol):
    properties = {
        'impossible_tasks': ListProperty(ImpossibleTask),
    }

class SessionStressInfo(Protocol):
    properties = {
        'acc_amount': ObjectProperty(Duration),
        'stress': ObjectProperty(Ratio),
    }

class SessionTypeEnum(Enum):
    TASK = 0
    FRAGMENT = 1

class SessionType(PrimitiveProtocol):
    def __init__(self, value = SessionTypeEnum.TASK):
        PrimitiveProtocol.__init__(self, value)

    def encode(self):
        return self.value.value

    def decode(self, encoded_protocol):
        self.value = SessionTypeEnum(encoded_protocol)

class Session(Protocol):
    properties = {
        'id': ObjectProperty(TaskID),
        'amount': ObjectProperty(Duration),
        'type': ObjectProperty(SessionType),
        'stress_info': ObjectProperty(SessionStressInfo),
    }

class DatedSession(Protocol):
    properties = {
        'date': ObjectProperty(Date),
        'session': ObjectProperty(Session),
    }

class DailyInfo(Protocol):
    properties = {
        'date': ObjectProperty(Date),
        'work_time': ObjectProperty(Duration),
        'sessions': ListProperty(Session),
        'free_time': ObjectProperty(Duration),
        'average_stress': ObjectProperty(Ratio),
    }

class PlannerResult(Protocol):
    properties = {
        'impossible_tasks': ListProperty(ImpossibleTask),
    }

class SchedulingResponse(Protocol):
    properties = {
        'general': ObjectProperty(SchedulingGeneralInfo),
        'alerts': ObjectProperty(Alerts),
        'daily_infos': ListProperty(DailyInfo),
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
        'free_time': ObjectProperty(Duration),
        'average_stress': ObjectProperty(Ratio),
    }

class DailyStressInfo(Protocol):
    properties = {
        'acc_free_time': ObjectProperty(Duration),
        'acc_average_stress': ObjectProperty(Ratio),
    }

class StressInfo(Protocol):
    '''
    TODO: This is not yet encodable, since it intends to use Date as key.
    '''
    properties = {
        'overall_stress': ObjectProperty(Ratio),
        'daily_stress_infos': DictProperty(DailyStressInfo), # use Date as key
    }
    

class FillDirection(Enum):
    EARLY = 0
    LATE = 1


class DailySchedule(object):

    def __init__(self, work_time):
        self._work_time = work_time
        self._used_time = 0
        self._sessions = []

    def copy(self):
        return copy.deepcopy(self)

    def get_work_time(self):
        return self._work_time

    def get_free_time(self):
        return self._work_time - self._used_time

    def add_session(self, session):
        task_id = session.id.value
        amount = session.amount.value
        if amount > self.get_free_time():
            raise ValueError()

        self._used_time += amount
        self._sessions.append(session)

    def get_sessions(self):
        return self._sessions


class Schedule(object):
    
    def __init__(self, schedule_start, schedule_end, daily_schedules):
        self.schedule_start = schedule_start
        self.schedule_end = schedule_end
        self.daily_schedules = daily_schedules

    def get_schedule_start(self):
        return self.schedule_start
    
    def get_schedule_end(self):
        return self.schedule_end
    
    def copy(self):
        schedule_start = self.schedule_start.copy()
        schedule_end = self.schedule_end.copy()
        daily_schedules = {}
        for date in self.daily_schedules:
            daily_schedules[date] = self.daily_schedules[date].copy()
            
        return Schedule(schedule_start, schedule_end, daily_schedules)

    def add_session(self, date, session):
        self.daily_schedules[date].add_session(session)

    def add_dated_sessions(self, dated_sessions):
        for dated_session in dated_sessions:
            date, session = dated_session.date, dated_session.session
            self.daily_schedules[date].add_session(session)
    
    def get_sessions(self, date):
        return self.daily_schedules[date].get_sessions()

    def get_work_time(self, date):
        return self.daily_schedules[date].get_work_time()

    def get_free_time(self, date):
        return self.daily_schedules[date].get_free_time()

    def from_work_time_dict(schedule_start, schedule_end, work_time_dict):
        daily_schedules = {}
        
        date = schedule_start
        while date <= schedule_end:
            daily_schedules[date] = DailySchedule(
                work_time = work_time_dict[date].value,
            )
            date = date.add_days(1)

        return Schedule(schedule_start, schedule_end, daily_schedules)

    def __str__(self):
        s = "".join([
            "{}: [{}]\n".format(date.encode(), self.get_free_time(date)) +
            "".join([
                "- {}: {}\n".format(session.id.value, session.amount.value)
                for session in self.get_sessions(date)
            ])
            for date in [
                self.schedule_start.add_days(i)
                for i in range(
                    self.schedule_start.days_to(self.schedule_end) + 1
                )
            ]
        ])
        return "==========\n" + s + "==========\n"


