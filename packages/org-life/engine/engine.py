import re

import util
from logger import DummyLogger
from protocol import *
from enum import Enum


class FillDirection(Enum):
    EARLY = 0
    LATE = 1


class WorkTimeParser(object):

    DEFAULT_SELECTOR = 'default'
    DEFAULT_WORK_TIME = Duration(8)
    SELECTOR_TO_WEEKDAY = {
        'monday': 0,
        'tuesday': 1,
        'wednesday': 2,
        'thursday': 3,
        'friday': 4,
        'saturday': 5,
        'sunday': 6,
    }

    SINGLE_DATE_RE = re.compile(r'^\d+-\d+-\d+$')
    DATE_RANGE_RE = re.compile(r'^\d+-\d+-\d+\s+-\s+\d+-\d+-\d+$')
    DATE_RANGE_SPLIT_RE = re.compile(r'\s+-\s+')

    def __init__(self):
        pass

    def _match_single_date(value):
        return WorkTimeParser.SINGLE_DATE_RE.match(value) is not None

    def _parse_single_date(value):
        components = [int(component) for component in value.split('-')]
        return Date.from_components(*components)

    def _match_date_range(value):
        return WorkTimeParser.DATE_RANGE_RE.match(value) is not None

    def _parse_date_range(value):
        dates = WorkTimeParser.DATE_RANGE_SPLIT_RE.split(value)
        return WorkTimeParser._parse_single_date(dates[0]), WorkTimeParser._parse_single_date(dates[1])

    def get_work_time_dict(self, schedule_start, schedule_end, work_time_config):
        '''
        TODO
        
        >>> parser = WorkTimeParser()
        >>> work_time_config = [
        ...     WorkTimeConfigEntry().decode_self(
        ...         {"selector":"default","duration":5}
        ...     ),
        ...     WorkTimeConfigEntry().decode_self(
        ...         {"selector":"saturday","duration":10}
        ...     ),
        ...     WorkTimeConfigEntry().decode_self(
        ...         {"selector":"friday","duration":20}
        ...     ),
        ...     WorkTimeConfigEntry().decode_self(
        ...         {"selector":"2018-12-06","duration":15}
        ...     ),
        ...     WorkTimeConfigEntry().decode_self(
        ...         {"selector":"2018-12-12 - 2018-12-16","duration":30}
        ...     ),
        ...     WorkTimeConfigEntry().decode_self(
        ...         {"selector":"2018-12-14","duration":0}
        ...     ),
        ... ]
        >>> schedule_start = Date().decode_self('2018-12-06')
        >>> schedule_end = Date().decode_self('2019-01-01')
        >>> d = parser.get_work_time_dict(schedule_start, schedule_end, work_time_config)
        >>> d[Date().decode_self('2018-12-06')].value
        15
        >>> d[Date().decode_self('2018-12-07')].value
        20
        >>> d[Date().decode_self('2018-12-08')].value
        10
        >>> d[Date().decode_self('2018-12-09')].value
        5
        >>> d[Date().decode_self('2018-12-11')].value
        5
        >>> d[Date().decode_self('2018-12-12')].value
        30
        >>> d[Date().decode_self('2018-12-13')].value
        30
        >>> d[Date().decode_self('2018-12-14')].value
        0
        >>> d[Date().decode_self('2018-12-15')].value
        30
        >>> d[Date().decode_self('2018-12-20')].value
        5
        '''
        default = Duration(WorkTimeParser.DEFAULT_WORK_TIME.value)
        result = {}
        day_of_week_dict = {}

        # parse day_of_week_dict
        for entry in work_time_config:
            if entry.selector.value == WorkTimeParser.DEFAULT_SELECTOR:
                default = Duration(entry.duration.value)

            elif entry.selector.value in WorkTimeParser.SELECTOR_TO_WEEKDAY:
                day_of_week_dict[
                    WorkTimeParser.SELECTOR_TO_WEEKDAY[entry.selector.value]
                ] = Duration(entry.duration.value)

        # default and weekday selectors
        date = schedule_start
        while date <= schedule_end:
            weekday = date.get_weekday()
            if weekday in day_of_week_dict:
                result[date] = Duration(day_of_week_dict[weekday].value)

            else:
                result[date] = Duration(default.value)

            date = date.add_days(1)

        # date selectors
        for entry in work_time_config:
            if WorkTimeParser._match_single_date(entry.selector.value):
                date = WorkTimeParser._parse_single_date(entry.selector.value)
                result[date] = Duration(entry.duration.value)

            elif WorkTimeParser._match_date_range(entry.selector.value):
                date_start, date_end = WorkTimeParser._parse_date_range(entry.selector.value)
                if date_start < schedule_start:
                    date_start = schedule_start

                if schedule_end < date_end:
                    date_end = schedule_end

                date = date_start
                while date <= date_end:
                    result[date] = Duration(entry.duration.value)
                    date = date.add_days(1)


        return result


class Schedule(object):
    
    def __init__(self, work_time_dict):
        raise NotImplementedError()

        self._invalidate_cache()
    
    def copy(self):
        raise NotImplementedError()
    
    def add_sessions(self, sessions):
        raise NotImplementedError()

        self._invalidate_cache()

    def _invalidate_cache(self):
        raise NotImplementedError()
    
    def get_sessions(self, date):
        raise NotImplementedError()

    def get_free_time_info(self, date):
        self._cache_free_time_info_if_needed()
        raise NotImplementedError()

    def get_impossible_tasks(self):
        raise NotImplementedError()

    def _cache_free_time_info_if_needed(self):
        raise NotImplementedError()

    def from_work_time_dict(schedule_start, schedule_end, work_time_dict):
        raise NotImplementedError()


class Planner(object):

    def __init__(self):
        pass

    def plan(self, tasks, schedule, direction = FillDirection.EARLY):
        raise NotImplementedError()


class StressAnalyzer(object):

    def __init__(self):
        pass

    def analyze(self, schedule):
        '''
        TODO return using Ratio protocol
        '''
        raise NotImplementedError()


class Fragmentizer(object):

    def __init__(self):
        pass

    def suggest_fragments(self, tasks, schedule):
        '''
        TODO set session type to SessionTypeEnum.FRAGMENT
        '''
        raise NotImplementedError()


class Engine(object):

    def __init__(self, work_time_parser, planner, stress_analyzer, fragmentizer, logger = DummyLogger()):
        self.work_time_parser = work_time_parser
        self.planner = planner
        self.stress_analyzer = stress_analyzer
        self.fragmentizer = fragmentizer
        self.logger = logger

    def create(logger = DummyLogger()):
        '''
        Factory method.
        '''
        work_time_parser = WorkTimeParser()
        planner = Planner()
        stress_analyzer = StressAnalyzer()
        fragmentizer = Fragmentizer()
        return Engine(work_time_parser, planner, stress_analyzer, fragmentizer, logger)

    def schedule(self, scheduling_request):
        # setup
        config = scheduling_request.config
        tasks = scheduling_request.tasks
        work_time_config = scheduling_request.work_time
        schedule_start = Date.today()
        schedule_end = schedule_start.add_days(config.scheduling_days - 1) # inclusive

        # results
        response = SchedulingResponse()
        for i in range(config.scheduling_days):
            daily_info = DailyInfo()
            daily_info.date = Date.today().add_days(i)
            response.daily_infos.append(daily_info)

        # parse work time config
        work_time_dict = self.work_time_parser.get_work_time_dict(schedule_start, schedule_end, work_time_config)
        
        # write to result
        for daily_info in response.daily_infos:
            daily_info.work_time = work_time_dict[daily_info.date]
        
        # schedule objects
        early_schedule = Schedule.from_work_time_dict(work_time_dict)
        late_schedule = early_schedule.copy()

        # progress count
        # right now we just use given "done" progress.
        # we add more future progress by doing the counts

        # free time and conflict check
        # run backward pass to generate maximum free time, and overall stress
        late_sessions = self.planner.plan(tasks, late_schedule, direction = FillDirection.LATE)
        late_schedule.add_sessions(late_sessions)
        impossible_tasks = late_schedule.get_impossible_tasks()
        # get free time info and stress
        for daily_info in response.daily_infos:
            free_time_info = late_schedule.get_free_time_info(daily_info.date)
            daily_info.free_time.value = free_time_info.free_time.value
            daily_info.average_stress.value = free_time_info.average_stress.value

        overall_stress = self.stress_analyzer.analyze(late_schedule)

        response.general.stress.value = overall_stress.value

        # report impossible tasks to alert
        response.alerts.impossible = impossible_tasks

        # schedule suggestion for fragmented tasks (today or more)
        # compute fragmented time for today (using maximum free time)
        # have the randomizer seeded with today's day string.
        # select task randomly from all tasks
        fragment_sessions = self.fragmentizer.suggest_fragments(tasks, schedule)

        # schedule suggestion for deadline tasks
        # run forward pass (while accounting for today's fragmented time) to generate suggestion
        early_schedule.add_sessions(fragment_sessions)
        early_sessions = self.planner.plan(tasks, early_schedule, direction = FillDirection.EARLY)
        early_schedule.add_sessions(early_sessions)
        
        # compute task stress of each session
        # TODO

        # write to response
        for daily_info in response.daily_infos:
            daily_info.sessions = early_schedule.get_sessions(daily_info.date)

        return response

def main():
    import doctest
    doctest.testmod()


if __name__ == '__main__':
    main()
