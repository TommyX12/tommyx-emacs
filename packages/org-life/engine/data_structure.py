import copy, calendar, math
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
        return prop.encode()

    def decode(self, encoded_prop):
        return self.prop_type().decode_self(encoded_prop)


class NullableObjectProperty(ObjectProperty):

    def __init__(self, prop_type, default_non_null = False):
        ObjectProperty.__init__(self, prop_type)
        self.default_non_null = default_non_null

    def get_default_value(self):
        if self.default_non_null:
            return self.prop_type()

        return None

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
        
    def encode(self):
        if self.value == math.inf:
            return 'inf'
        
        elif self.value == -math.inf:
            return '-inf'

        else:
            return self.value

    def decode(self, encoded_protocol):
        if encoded_protocol == 'inf':
            self.value = math.inf
        
        elif encoded_protocol == '-inf':
            self.value = -math.inf

        else:
            self.value = encoded_protocol

class Days(PrimitiveProtocol):
    def __init__(self, value = 0):
        PrimitiveProtocol.__init__(self, value)

class TaskID(PrimitiveProtocol):
    def __init__(self, value = 0):
        PrimitiveProtocol.__init__(self, value)

class String(PrimitiveProtocol):
    def __init__(self, value = ""):
        PrimitiveProtocol.__init__(self, value)

class TaskIndex(PrimitiveProtocol):
    def __init__(self, value = 0):
        PrimitiveProtocol.__init__(self, value)

class Priority(PrimitiveProtocol):
    def __init__(self, value = 0):
        PrimitiveProtocol.__init__(self, value)
        
    def decode(self, encoded_protocol):
        if encoded_protocol is None:
            self.value = math.inf

        else:
            self.value = encoded_protocol

class Boolean(PrimitiveProtocol):
    def __init__(self, value = False):
        PrimitiveProtocol.__init__(self, value)

    def decode(self, encoded_protocol):
        if encoded_protocol is None:
            self.value = False

        else:
            self.value = encoded_protocol


class Date(Protocol):
    '''
    TODO: Add tests.
    '''

    def __init__(self, date_object = None):
        Protocol.__init__(self)
        if date_object is None:
            date_object = DatetimeDate.today()

        self._date = date_object
        
    def encode(self):
        return '-'.join([str(x) for x in [self._date.year, self._date.month, self._date.day]])

    def from_components(year, month, day):
        return Date(DatetimeDate(year, month, day))

    def min():
        return Date(DatetimeDate.min)

    def max():
        return Date(DatetimeDate.max)

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

    def add_months(self, months):
        new_year = self._date.year + ((self._date.month + months - 1) // 12)
        new_months = ((self._date.month + months - 1) % 12) + 1
        new_day = min(self._date.day, calendar.monthrange(new_year, new_months)[1])
        return Date.from_components(new_year, new_months, new_day)

    def add_years(self, years):
        new_year = self._date.year + years
        new_months = self._date.month
        new_day = min(self._date.day, calendar.monthrange(new_year, new_months)[1])
        return Date.from_components(new_year, new_months, new_day)

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
        'min_extra_time_ratio': ObjectProperty(Ratio),
        'max_percentage': ObjectProperty(Ratio),
        'preferred_fragment_size': ObjectProperty(Duration),
        'min_fragment_size': ObjectProperty(Duration),
    }

class Config(Protocol):
    properties = {
        'show_debug_messages': ObjectProperty(Boolean),
        'today': ObjectProperty(Date),
        'scheduling_days': ObjectProperty(Days),
        'daily_info_days': ObjectProperty(Days),
        'fragmentation_config': ObjectProperty(FragmentationConfig),
    }

class TaskStatusEnum(Enum):
    TODO = 0
    DONE = 1

class TaskStatus(PrimitiveProtocol):
    def __init__(self, value = TaskStatusEnum.TODO):
        PrimitiveProtocol.__init__(self, value)

    def encode(self):
        return self.value.value

    def decode(self, encoded_protocol):
        self.value = TaskStatusEnum(encoded_protocol)

class TaskRepeatTypeEnum(Enum):
    NORMAL = 0
    RESTART = 1

class TaskRepeatType(PrimitiveProtocol):
    def __init__(self, value = TaskRepeatTypeEnum.NORMAL):
        PrimitiveProtocol.__init__(self, value)

    def encode(self):
        return self.value.value

    def decode(self, encoded_protocol):
        self.value = TaskRepeatTypeEnum(encoded_protocol)

class TaskRepeatUnitEnum(Enum):
    NONE = 0
    DAY = 1
    WEEK = 2
    MONTH = 3
    YEAR = 4

class TaskRepeatUnit(PrimitiveProtocol):
    def __init__(self, value = TaskRepeatUnitEnum.DAY):
        PrimitiveProtocol.__init__(self, value)

    def encode(self):
        return self.value.value

    def decode(self, encoded_protocol):
        self.value = TaskRepeatUnitEnum(encoded_protocol)

class TaskRepeatValue(PrimitiveProtocol):
    def __init__(self, value = 1):
        PrimitiveProtocol.__init__(self, value)

class TaskRepeat(Protocol):
    properties = {
        'type': ObjectProperty(TaskRepeatType),
        'unit': ObjectProperty(TaskRepeatUnit),
        'value': ObjectProperty(TaskRepeatValue),
    }

class Task(Protocol):
    properties = {
        'id': ObjectProperty(TaskID),
        'start': ObjectProperty(Date),
        'end': ObjectProperty(Date),
        'amount': NullableObjectProperty(Duration, True),
        'done': ObjectProperty(Duration),
        'status': ObjectProperty(TaskStatus),
        'priority': ObjectProperty(Priority),
        'repeat': NullableObjectProperty(TaskRepeat),
        'stressless': ObjectProperty(Boolean),
    }

    def copy(self):
        return copy.deepcopy(self)
    
    def decode(self, encoded_protocol):
        Protocol.decode(self, encoded_protocol)
        if self.start > self.end:
            self.start = self.end

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
    OVERLIMIT = 2

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
        'task_index': NullableObjectProperty(TaskIndex),
        'to_finish': NullableObjectProperty(Duration),
        'to_deadline': NullableObjectProperty(Days),
        # 'stress_info': ObjectProperty(SessionStressInfo),
    }

    def copy(self):
        return copy.deepcopy(self)

    def with_weakness(self, weakness_value):
        new_session = self.copy()
        new_session.weakness.value = weakness_value
        return new_session

class DatedSession(Protocol):
    properties = {
        'date': ObjectProperty(Date),
        'session': ObjectProperty(Session),
    }

    def __init__(self, date = None, session = None):
        Protocol.__init__(self)
        if date is not None:
            self.date = date
        if session is not None:
            self.session = session

    def copy(self):
        return copy.deepcopy(self)

    def with_weakness(self, weakness_value):
        new_dated_session = self.copy()
        new_dated_session.session.weakness.value = weakness_value
        return new_dated_session

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
        'extra_time_ratio': ObjectProperty(Ratio),
        'highest_stress_date': ObjectProperty(Date),
        'highest_stress_task': NullableObjectProperty(TaskID),
        'stress_with_optimal': ObjectProperty(Ratio),
        'stress_with_suggested': ObjectProperty(Ratio),
        'stress_without_today': ObjectProperty(Ratio),
        'etr_with_optimal': ObjectProperty(Ratio),
        'etr_with_suggested': ObjectProperty(Ratio),
        'etr_without_today': ObjectProperty(Ratio),
    }

class ImpossibleTask(Protocol):
    properties = {
        'id': ObjectProperty(TaskID),
        'amount': ObjectProperty(Duration),
    }

class BadEstimateTask(Protocol):
    properties = {
        'id': ObjectProperty(TaskID),
        'amount': ObjectProperty(Duration),
        'done': ObjectProperty(Duration),
    }

class BadInfoTask(Protocol):
    properties = {
        'id': ObjectProperty(TaskID),
        'reason': ObjectProperty(String),
    }

class OverdueTask(Protocol):
    properties = {
        'id': ObjectProperty(TaskID),
        'days': ObjectProperty(Days),
    }

class Alerts(Protocol):
    properties = {
        'impossible_tasks': ListProperty(ImpossibleTask),
        'bad_estimate_tasks': ListProperty(BadEstimateTask),
        'bad_info_tasks': ListProperty(BadInfoTask),
        'overdue_tasks': ListProperty(OverdueTask),
    }

class DailyInfo(Protocol):
    properties = {
        'date': ObjectProperty(Date),
        'usable_time': ObjectProperty(Duration),
        'actual_usable_time': ObjectProperty(Duration),
        'sessions': ListProperty(Session),
        'free_time': ObjectProperty(Duration),
        'average_stress': ObjectProperty(Ratio),
        'average_etr': ObjectProperty(Ratio),
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
        'acc_extra_time_ratio': ObjectProperty(Ratio),
    }

class StressInfo(Protocol):
    '''
    TODO: This is not yet encodable, since it intends to use Date as key.
    '''
    properties = {
        'extra_time_ratio': ObjectProperty(Ratio),
        'overall_stress': ObjectProperty(Ratio),
        'highest_stress_date': ObjectProperty(Date),
        'daily_stress_infos': DictProperty(DailyStressInfo), # use Date as key
    }


class TaskProressInfo(Protocol):
    properties = {
        'done_amount': ObjectProperty(Duration),
    }

    def __init__(self, initial_progress = 0):
        Protocol.__init__(self)
        self.done_amount.value = initial_progress
    
class ProgressInfo(Protocol):
    '''
    TODO: This is not yet encodable, since it intends to use task id as key.
    '''
    properties = {
        'tasks_progress': ListProperty(TaskProressInfo), # use task id as key
    }

    def __init__(self, num_tasks):
        Protocol.__init__(self)
        self.tasks_progress = [TaskProressInfo(0) for _ in range(num_tasks)]

    def copy(self):
        return copy.deepcopy(self)

    def get_done_amount(self, task_index):
        return self.tasks_progress[task_index].done_amount.value
    
    def add_done_amount(self, task_index, done_amount):
        self.tasks_progress[task_index].done_amount.value += done_amount
    
    def set_done_amount(self, task_index, done_amount):
        self.tasks_progress[task_index].done_amount.value = done_amount

    def combine(self, progress_info):
        result = self.copy()
        for task_index in range(len(self.tasks_progress)):
            task_progress = progress_info.tasks_progress[task_index]
            result.add_done_amount(task_index, task_progress.done_amount.value)

        return result
    

class FillDirection(Enum):
    EARLY = 0
    LATE = 1

class SessionOrder(Enum):
    NONE = 0
    AMOUNT = 1

class DailySchedule(object):

    def __init__(self, usable_time):
        self._real_usable_time = usable_time
        self._usable_time = usable_time
        self._used_time = 0
        self._sessions = []
        self._id_to_session = {
            SessionWeaknessEnum.STRONG: {
                SessionTypeEnum.TASK: {},
                SessionTypeEnum.FRAGMENT: {},
                SessionTypeEnum.OVERLIMIT: {},
            },
            SessionWeaknessEnum.WEAK: {
                SessionTypeEnum.TASK: {},
                SessionTypeEnum.FRAGMENT: {},
                SessionTypeEnum.OVERLIMIT: {},
            },
        }

    def copy(self):
        return copy.deepcopy(self)

    def get_usable_time(self):
        return self._usable_time

    def get_real_usable_time(self):
        return self._real_usable_time

    def get_free_time(self):
        return max(0, self._usable_time - self._used_time)

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

    def get_sessions(self, order = None):
        if order == SessionOrder.AMOUNT:
            return sorted(self._sessions,
                          key = lambda x : x.amount.value,
                          reverse = True)

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
    
    def copy(self, schedule_start = None, schedule_end = None):
        if schedule_start is None:
            schedule_start = self.schedule_start.copy()
            
        if schedule_end is None:
            schedule_end = self.schedule_end.copy()
        
        daily_schedules = {}
        date = schedule_start
        while date <= schedule_end:
            daily_schedules[date] = self.daily_schedules[date].copy()
            date = date.add_days(1)
            
        return Schedule(schedule_start, schedule_end, daily_schedules)

    def add_session(self, date, session):
        if date in self.daily_schedules:
            self.daily_schedules[date].add_session(session)

    def add_sessions(self, date, sessions):
        for session in sessions:
            self.add_session(date, session)

    def add_dated_sessions(self, dated_sessions):
        for dated_session in dated_sessions:
            date, session = dated_session.date, dated_session.session
            if date in self.daily_schedules:
                self.daily_schedules[date].add_session(session)
    
    def get_sessions(self, date, order = None):
        return self.daily_schedules[date].get_sessions(order)

    def get_all_sessions(self, from_date = None, to_date = None, order = None):
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
            sessions += self.daily_schedules[date].get_sessions(order)
            date = date.add_days(1)

        return sessions

    def is_overlimit(self, date):
        return self.daily_schedules[date].is_overlimit()

    def get_usable_time(self, date):
        return self.daily_schedules[date].get_usable_time()

    def get_real_usable_time(self, date):
        return self.daily_schedules[date].get_real_usable_time()

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


