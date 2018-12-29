import re
from enum import Enum

import util
from logger import DummyLogger
from data_structure import *
from usable_time_parser import UsableTimeParser
from task_filter import TaskFilter
from task_repeater import TaskRepeater
from progress_counter import ProgressCounter
from planner import Planner
from stress_analyzer import StressAnalyzer
from fragmentizer import Fragmentizer


class Engine(object):

    def __init__(self, usable_time_parser, task_filter, task_repeater, progress_counter, planner, stress_analyzer, fragmentizer, logger = DummyLogger()):
        self.usable_time_parser = usable_time_parser
        self.task_filter = task_filter
        self.task_repeater = task_repeater
        self.progress_counter = progress_counter
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
        task_repeater = TaskRepeater()
        progress_counter = ProgressCounter()
        planner = Planner()
        stress_analyzer = StressAnalyzer()
        fragmentizer = Fragmentizer()
        return Engine(usable_time_parser, task_filter, task_repeater, progress_counter, planner, stress_analyzer, fragmentizer, logger)

    def schedule(self, scheduling_request):
        # setup
        config = scheduling_request.config
        schedule_start = config.today
        schedule_end = schedule_start.add_days(config.scheduling_days.value - 1) # inclusive
        tasks = scheduling_request.tasks
        dated_sessions = scheduling_request.dated_sessions
        dated_sessions.sort(key = lambda x : x.date)
        usable_time_config = scheduling_request.usable_time

        response = SchedulingResponse()
        for i in range(config.daily_info_days.value):
            daily_info = DailyInfo()
            daily_info.date = schedule_start.add_days(i)
            response.daily_infos.append(daily_info)

        # parse work time config
        usable_time_dict = self.usable_time_parser.get_usable_time_dict(schedule_start, schedule_end, usable_time_config)
        
        for daily_info in response.daily_infos:
            daily_info.usable_time = usable_time_dict[daily_info.date]

        # task repeat
        tasks = self.task_repeater.repeat(tasks, schedule_start, schedule_end)
        
        # progress count
        strong_dated_sessions = [
            dated_session for dated_session in dated_sessions
            if dated_session.session.weakness.value == SessionWeaknessEnum.STRONG
        ]
        strong_dated_sessions_without_today = [
            dated_session for dated_session in strong_dated_sessions
            if dated_session.date != schedule_start
        ]
        strong_dated_sessions_today = [
            dated_session for dated_session in strong_dated_sessions
            if dated_session.date == schedule_start
        ]
        weak_dated_sessions = [
            dated_session for dated_session in dated_sessions
            if dated_session.session.weakness.value == SessionWeaknessEnum.WEAK
        ]
        strong_progress_without_today = self.progress_counter.count(tasks, strong_dated_sessions_without_today, sessions_sorted = True)
        strong_progress_today = self.progress_counter.count(tasks, strong_dated_sessions_today, sessions_sorted = True)
        strong_progress = strong_progress_without_today.combine(strong_progress_today)

        # make schedule objects
        early_schedule = Schedule.from_usable_time_dict(schedule_start, schedule_end, usable_time_dict)
        early_schedule.add_dated_sessions(strong_dated_sessions)
        late_schedule = early_schedule.copy()

        # impossible tasks check
        stress_contributor_tasks_mask = self.task_filter.get_stress_contributor_tasks_mask(tasks, schedule_start, schedule_end)
        late_plan_result = self.planner.plan(
            tasks,
            late_schedule,
            direction = FillDirection.LATE,
            progress_info = strong_progress,
            tasks_mask = stress_contributor_tasks_mask
        )
        impossible_tasks = late_plan_result.impossible_tasks
        
        # free time info and stress
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
        late_schedule_with_optimal = early_schedule.copy()
        self.planner.plan(
            tasks,
            late_schedule_with_optimal,
            direction = FillDirection.EARLY,
            progress_info = strong_progress,
            early_stop = schedule_start,
            session_weakness = SessionWeaknessEnum.STRONG
        )
        optimal_dated_sessions = [
            DatedSession(schedule_start, session)
            for session in
            late_schedule_with_optimal.get_sessions(schedule_start)
        ]
        strong_progress_with_optimal = self.progress_counter.count(tasks, optimal_dated_sessions, sessions_sorted = True)
        strong_progress_with_optimal = strong_progress_with_optimal.combine(strong_progress_without_today)
        self.planner.plan(
            tasks,
            late_schedule_with_optimal,
            direction = FillDirection.LATE,
            progress_info = strong_progress_with_optimal,
            tasks_mask = stress_contributor_tasks_mask
        )
        
        late_schedule_with_suggested = early_schedule.copy()
        strong_fragment_sessions = [
            dated_session.with_weakness(SessionWeaknessEnum.STRONG)
            for dated_session in fragment_sessions
        ]
        late_schedule_with_suggested.add_dated_sessions(strong_fragment_sessions)
        self.planner.plan(
            tasks,
            late_schedule_with_suggested,
            direction = FillDirection.EARLY,
            progress_info = strong_progress,
            early_stop = schedule_start,
            session_weakness = SessionWeaknessEnum.STRONG
        )
        suggested_dated_sessions = [
            DatedSession(schedule_start, session)
            for session in
            late_schedule_with_suggested.get_sessions(schedule_start)
        ]
        strong_progress_with_suggested = self.progress_counter.count(tasks, suggested_dated_sessions, sessions_sorted = True)
        strong_progress_with_suggested = strong_progress_with_suggested.combine(strong_progress_without_today)
        self.planner.plan(
            tasks,
            late_schedule_with_suggested,
            direction = FillDirection.LATE,
            progress_info = strong_progress_with_suggested,
            tasks_mask = stress_contributor_tasks_mask
        )
        
        stress_info_with_optimal = self.stress_analyzer.analyze(late_schedule_with_optimal)
        response.general.stress_with_optimal.value = stress_info_with_optimal.overall_stress.value
        
        stress_info_with_suggested = self.stress_analyzer.analyze(late_schedule_with_suggested)
        response.general.stress_with_suggested.value = stress_info_with_suggested.overall_stress.value
        
        stress_info_without_today = self.stress_analyzer.analyze(late_schedule, bias = late_schedule.get_usable_time(schedule_start))
        response.general.stress_without_today.value = stress_info_without_today.overall_stress.value

        # schedule suggestion for deadline tasks
        # run forward pass (while accounting for today's fragmented time) to generate suggestion
        early_schedule.add_dated_sessions(fragment_sessions)
        self.planner.plan(tasks, early_schedule, direction = FillDirection.EARLY, progress_info = strong_progress)
        
        # compute task stress of each session
        # TODO

        for daily_info in response.daily_infos:
            daily_info.sessions = early_schedule.get_sessions(daily_info.date)

        return response
    

def main():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    main()
