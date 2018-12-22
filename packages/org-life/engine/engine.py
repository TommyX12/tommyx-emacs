import re
from enum import Enum

import util
from logger import DummyLogger
from data_structure import *
from usable_time_parser import UsableTimeParser
from task_filter import TaskFilter
from planner import Planner
from stress_analyzer import StressAnalyzer
from fragmentizer import Fragmentizer


class Engine(object):

    def __init__(self, usable_time_parser, task_filter, planner, stress_analyzer, fragmentizer, logger = DummyLogger()):
        self.usable_time_parser = usable_time_parser
        self.task_filter = task_filter
        self.planner = planner
        self.stress_analyzer = stress_analyzer
        self.fragmentizer = fragmentizer
        self.logger = logger

    def create(logger = DummyLogger()):
        '''
        Factory method.
        '''
        usable_time_parser = UsableTimeParser()
        task_filter = TaskFilter()
        planner = Planner()
        stress_analyzer = StressAnalyzer()
        fragmentizer = Fragmentizer()
        return Engine(usable_time_parser, task_filter, planner, stress_analyzer, fragmentizer, logger)

    def schedule(self, scheduling_request):
        # setup
        config = scheduling_request.config
        tasks = scheduling_request.tasks
        sessions = scheduling_request.sessions
        usable_time_config = scheduling_request.usable_time
        schedule_start = config.today
        schedule_end = schedule_start.add_days(config.scheduling_days.value - 1) # inclusive

        # results
        response = SchedulingResponse()
        for i in range(config.daily_info_days.value):
            daily_info = DailyInfo()
            daily_info.date = schedule_start.add_days(i)
            response.daily_infos.append(daily_info)

        # parse work time config
        usable_time_dict = self.usable_time_parser.get_usable_time_dict(schedule_start, schedule_end, usable_time_config)
        
        # write to result
        for daily_info in response.daily_infos:
            daily_info.usable_time = usable_time_dict[daily_info.date]
        
        # make schedule objects
        early_schedule = Schedule.from_usable_time_dict(schedule_start, schedule_end, usable_time_dict)
        late_schedule = early_schedule.copy()

        # progress count
        # right now we just use given "done" progress.
        # we add more future progress by doing the counts

        # free time and conflict check
        # run backward pass to generate maximum free time, and overall stress
        stress_contributor_tasks = self.task_filter.get_stress_contributor_tasks(tasks, schedule_start, schedule_end)
        late_plan_result = self.planner.plan(stress_contributor_tasks, late_schedule, direction = FillDirection.LATE)
        impossible_tasks = late_plan_result.impossible_tasks
        # get free time info and stress
        stress_info = self.stress_analyzer.analyze(late_schedule)
        response.general.stress.value = stress_info.overall_stress.value
        response.general.highest_stress_date = stress_info.highest_stress_date
        for daily_info in response.daily_infos:
            daily_stress_info = stress_info.daily_stress_infos[daily_info.date]
            daily_info.free_time.value = daily_stress_info.acc_free_time.value
            daily_info.average_stress.value = daily_stress_info.acc_average_stress.value

        # report impossible tasks to alert
        response.alerts.impossible_tasks = impossible_tasks

        # schedule suggestion for fragmented tasks (today or more)
        # compute fragmented time for today (using maximum free time)
        # have the randomizer seeded with today's day string.
        # select task randomly from all tasks
        fragment_sessions = self.fragmentizer.suggest_fragments(
            tasks,
            late_schedule,
            stress_info,
            config.fragmentation_config
        )
        fragment_amount = Schedule.get_dated_sessions_amount(fragment_sessions)

        # additional stress info
        stress_info_with_fragment = self.stress_analyzer.analyze(late_schedule, bias = fragment_amount)
        response.general.stress_with_fragments.value = stress_info_with_fragment.overall_stress.value
        
        stress_info_without_today = self.stress_analyzer.analyze(late_schedule, bias = usable_time_dict[schedule_start].value)
        response.general.stress_without_today.value = stress_info_without_today.overall_stress.value

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
