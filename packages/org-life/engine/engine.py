import util
from logger import DummyLogger
from protocol import *


class FillDirection(enum):
    EARLY = 0
    LATE = 1


class WorkTimeParser(object):

    def __init__(self):
        pass

    def parse_work_time_config(schedule_start, schedule_end, work_time_config):
        raise NotImplementedError()


class Fragmentizer(object):

    def __init__(self):
        pass

    def suggest_fragment_tasks(self, tasks, schedule):
        raise NotImplementedError()


class Schedule(object):
    
    def __init__(self, work_time_list):
        raise NotImplementedError()

        self._invalidate_cache()
    
    def copy(self):
        raise NotImplementedError()
    
    def plan(self, tasks, direction = FillDirection.EARLY):
        raise NotImplementedError()

        self._invalidate_cache()

    def _invalidate_cache(self):
        raise NotImplementedError()
    
    def get_free_time_info(self, date):
        self._cache_free_time_info_if_needed()
        raise NotImplementedError()

    def get_overall_stress(self):
        self._cache_free_time_info_if_needed()
        raise NotImplementedError()

    def get_impossible_tasks(self):
        raise NotImplementedError()

    def _cache_free_time_info_if_needed(self):
        raise NotImplementedError()

    def from_work_time_list(work_time_list):
        raise NotImplementedError()

    def from_work_time_config(work_time_parser, schedule_start, schedule_end, work_time_config):
        work_time_list = work_time_parser.parse_work_time_config(schedule_start, schedule_end, work_time_config)
        return Schedule.from_work_time_list(work_time_list)
    

class Engine(object):

    def __init__(self, work_time_parser, fragmentizer, logger = DummyLogger()):
        self.work_time_parser = work_time_parser
        self.fragmentizer = fragmentizer
        self.logger = logger

    def create(self, logger = DummyLogger()):
        '''
        Factory method.
        '''
        work_time_parser = WorkTimeParser()
        fragmentizer = Fragmentizer()
        return Engine(work_time_parser, fragmentizer, logger)

    def schedule(self, scheduling_request):
        # setup
        config = scheduling_request.config
        tasks = scheduling_request.tasks
        work_time_config = scheduling_request.work_time
        schedule_start = Date.today()
        schedule_end = schedule_start.add_days(config.scheduling_days - 1)

        # results
        response = SchedulingResponse()
        for i in range(config.scheduling_days):
            response.daily_infos.append(DailyInfo())
        
        # schedule objects
        early_schedule = Schedule.from_work_time_config(self.work_time_parser, schedule_start, schedule_end, work_time_config)
        late_schedule = early_schedule.copy()

        # progress count
        # right now we just use given "done" progress.
        # we add more future progress by doing the counts

        # free time and conflict check
        # run backward pass to generate maximum free time, and overall stress
        late_schedule.plan(tasks, direction = FillDirection.LATE)
        impossible_tasks = late_schedule.get_impossible_tasks()
        # get free time info and stress
        for daily_info in response.daily_infos:
            free_time_info = late_schedule.get_free_time_info(daily_info.date)
            daily_info.free_time.value = free_time_info.free_time.value
            daily_info.average_stress.value = free_time_info.average_stress.value

        response.general.stress.value = late_schedule.get_overall_stress()

        # report impossible tasks to alert
        response.alerts.impossible = impossible_tasks

        # schedule suggestion for fragmented tasks (today or more)
        # compute fragmented time for today (using maximum free time)
        # have the randomizer seeded with today's day string.
        # select task randomly from all tasks
        self.fragmentizer.suggest_fragment_tasks(tasks, schedule)

        # schedule suggestion for deadline tasks, as well as task stress
        # run forward pass (while accounting for today's fragmented time) to generate suggestion
        # compute task stress of each session

