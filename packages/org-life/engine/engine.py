import re
from enum import Enum

import util
from logger import DummyLogger
from data_structure import *
from work_time_parser import WorkTimeParser
from task_filter import TaskFilter
from planner import Planner
from stress_analyzer import StressAnalyzer


class Engine(object):

    def __init__(self, work_time_parser, task_filter, planner, stress_analyzer, fragmentizer, logger = DummyLogger()):
        self.work_time_parser = work_time_parser
        self.task_filter = task_filter
        self.planner = planner
        self.stress_analyzer = stress_analyzer
        self.fragmentizer = fragmentizer
        self.logger = logger

    def create(logger = DummyLogger()):
        '''
        Factory method.
        '''
        work_time_parser = WorkTimeParser()
        task_filter = TaskFilter()
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
        
        # make schedule objects
        early_schedule = Schedule.from_work_time_dict(work_time_dict)
        late_schedule = early_schedule.copy()

        # progress count
        # right now we just use given "done" progress.
        # we add more future progress by doing the counts

        # free time and conflict check
        # run backward pass to generate maximum free time, and overall stress
        stress_contributor_tasks = self.task_filter.get_stress_contributor_tasks(tasks)
        late_plan_result = self.planner.plan(stress_contributor_tasks, late_schedule, direction = FillDirection.LATE)
        impossible_tasks = late_plan_result.impossible_tasks
        # get free time info and stress
        stress_info = self.stress_analyzer.analyze(late_schedule)
        response.general.stress.value = stress_info.overall_stress.value
        for daily_info in response.daily_infos:
            daily_stress_info = stress_info.daily_stress_infos[daily_info.date]
            daily_info.free_time.value = daily_stress_info.acc_free_time.value
            daily_info.average_stress.value = daily_stress_info.acc_average_stress.value

        # report impossible tasks to alert
        response.alerts.impossible = impossible_tasks

        # schedule suggestion for fragmented tasks (today or more)
        # compute fragmented time for today (using maximum free time)
        # have the randomizer seeded with today's day string.
        # select task randomly from all tasks
        fragment_sessions = self.fragmentizer.suggest_fragments(
            tasks,
            schedule,
            stress_info,
            config.fragmentation_config
        )

        # schedule suggestion for deadline tasks
        # run forward pass (while accounting for today's fragmented time) to generate suggestion
        early_schedule.add_dated_sessions(fragment_sessions)
        self.planner.plan(tasks, early_schedule, direction = FillDirection.EARLY)
        
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
