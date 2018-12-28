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
        ] if props is not None else []

    def decode(self, encoded_props):
        return [
            self.prop_type().decode_self(encoded_prop)
            for encoded_prop in encoded_props
        ] if encoded_props is not None else []

    
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
        } if props is not None else {}

    def decode(self, encoded_props):
        return {
            prop_name: self.prop_type().decode_self(encoded_props[prop_name])
            for prop_name in encoded_props
        } if encoded_props is not None else {}


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

    def is_min(self):
        return self._date == DatetimeDate.min

    def is_max(self):
        return self._date == DatetimeDate.max

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
        'preferred_fragment_size': ObjectProperty(Duration),
        'min_fragment_size': ObjectProperty(Duration),
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

class UsableTimeSelector(PrimitiveProtocol):
    def __init__(self, value = 'default'):
        PrimitiveProtocol.__init__(self, value)
        
    def encode(self):
        return self.value

    def decode(self, encoded_protocol):
        self.value = encoded_protocol.lower()

class UsableTimeConfigEntry(Protocol):
    properties = {
        'selector': ObjectProperty(UsableTimeSelector),
        'duration': ObjectProperty(Duration),
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

class SessionWeaknessEnum(Enum):
    STRONG = 0
    WEAK = 1

class SessionWeakness(PrimitiveProtocol):
    def __init__(self, value = SessionWeaknessEnum.WEAK):
        PrimitiveProtocol.__init__(self, value)

    def encode(self):
        return self.value.value

    def decode(self, encoded_protocol):
        self.value = SessionWeaknessEnum(encoded_protocol)

class Session(Protocol):
    properties = {
        'id': ObjectProperty(TaskID),
        'amount': ObjectProperty(Duration),
        'type': ObjectProperty(SessionType),
        'weakness': ObjectProperty(SessionWeakness),
        # 'stress_info': ObjectProperty(SessionStressInfo),
    }

    def copy(self):
        return copy.deepcopy(self)

class DatedSession(Protocol):
    properties = {
        'date': ObjectProperty(Date),
        'session': ObjectProperty(Session),
    }

class SchedulingRequest(Protocol):
    properties = {
        'config': ObjectProperty(Config),
        'tasks': ListProperty(Task),
        'dated_sessions': ListProperty(DatedSession),
        'usable_time': ListProperty(UsableTimeConfigEntry),
    }

class SchedulingGeneralInfo(Protocol):
    properties = {
        'stress': ObjectProperty(Ratio),
        'highest_stress_date': ObjectProperty(Date),
        'stress_with_fragments': ObjectProperty(Ratio),
        'stress_without_today': ObjectProperty(Ratio),
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

class DailyInfo(Protocol):
    properties = {
        'date': ObjectProperty(Date),
        'usable_time': ObjectProperty(Duration),
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
        'debug': PrimitiveProperty(),
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
        'highest_stress_date': ObjectProperty(Date),
        'daily_stress_infos': DictProperty(DailyStressInfo), # use Date as key
    }


class TaskProressInfo(Protocol):
    properties = {
        'done_amount': ObjectProperty(Duration),
    }
    
class ProgressInfo(Protocol):
    '''
    TODO: This is not yet encodable, since it intends to use task id as key.
    '''
    properties = {
        'tasks_progress': DictProperty(TaskProressInfo), # use task id as key
    }

    def copy(self):
        return copy.deepcopy(self)

    def _ensure_exist(self, task_id):
        if task_id not in self.tasks_progress:
            task_progress =  TaskProressInfo()
            task_progress.done_amount.value = 0
            self.tasks_progress[task_id] = task_progress

    def get_done_amount(self, task_id):
        self._ensure_exist(task_id)
        return self.tasks_progress[task_id].done_amount.value
    
    def add_done_amount(self, task_id, done_amount):
        self._ensure_exist(task_id)
        self.tasks_progress[task_id].done_amount.value += done_amount
    
    def set_done_amount(self, task_id, done_amount):
        self._ensure_exist(task_id)
        self.tasks_progress[task_id].done_amount.value = done_amount

    def combine(self, progress_info):
        result = self.copy()
        for task_id in progress_info.tasks_progress:
            task_progress = progress_info.tasks_progress[task_id]
            result.add_done_amount(task_id, task_progress.done_amount.value)

        return result
    

class FillDirection(Enum):
    EARLY = 0
    LATE = 1


class DailySchedule(object):

    def __init__(self, usable_time):
        self._real_usable_time = usable_time
        self._usable_time = usable_time
        self._used_time = 0
        self._sessions = []
        self._id_to_session = {
            SessionWeaknessEnum.STRONG: {
                SessionTypeEnum.TASK: {
                    
                },
                SessionTypeEnum.FRAGMENT: {
                    
                },
            },
            SessionWeaknessEnum.WEAK: {
                SessionTypeEnum.TASK: {
                    
                },
                SessionTypeEnum.FRAGMENT: {
                    
                },
            },
        }

    def copy(self):
        return copy.deepcopy(self)

    def get_usable_time(self):
        return self._usable_time

    def get_free_time(self):
        return self._usable_time - self._used_time

    def is_overlimit(self):
        return self._used_time > self._usable_time

    def add_session(self, session):
        task_id = session.id.value
        amount = session.amount.value
        if session.weakness.value == SessionWeaknessEnum.STRONG:
            self._usable_time = max(0, self._usable_time - amount)

        else:
            self._used_time += amount

        id_to_session_dict = self._id_to_session[session.weakness.value][session.type.value]
        if task_id in id_to_session_dict:
            id_to_session_dict[task_id].amount.value += amount
            
        else:
            session = session.copy()
            self._sessions.append(session)
            id_to_session_dict[task_id] = session
        
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
            if date in self.daily_schedules:
                self.daily_schedules[date].add_session(session)
    
    def get_sessions(self, date):
        return self.daily_schedules[date].get_sessions()

    def get_all_sessions(self, from_date = None, to_date = None):
        '''
        Return a list of all sessions from from_date to to_date (inclusive).
        '''
        if from_date is None:
            from_date = self.schedule_start

        if to_date is None:
            to_date = self.schedule_end

        sessions = []
        date = from_date
        while date <= to_date:
            sessions += self.daily_schedules[date].get_sessions()
            date = date.add_days(1)

        return sessions

    def is_overlimit(self, date):
        return self.daily_schedules[date].is_overlimit()

    def get_usable_time(self, date):
        return self.daily_schedules[date].get_usable_time()

    def get_free_time(self, date):
        return self.daily_schedules[date].get_free_time()

    def from_usable_time_dict(schedule_start, schedule_end, usable_time_dict):
        daily_schedules = {}
        
        date = schedule_start
        while date <= schedule_end:
            daily_schedules[date] = DailySchedule(
                usable_time = usable_time_dict[date].value,
            )
            date = date.add_days(1)

        return Schedule(schedule_start, schedule_end, daily_schedules)

    def get_dated_sessions_amount(dated_sessions):
        result = 0
        for dated_session in dated_sessions:
            result += dated_session.session.amount.value

        return result

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


