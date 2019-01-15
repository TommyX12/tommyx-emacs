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
        # debug
        debug_timer = util.PerformanceTimer()
        debug_timer.push()

        # setup
        debug_timer.push()

        config = scheduling_request.config
        schedule_start = config.today
        schedule_end = schedule_start.add_days(config.scheduling_days.value - 1) # inclusive
        daily_info_end = schedule_start.add_days(config.daily_info_days.value - 1) # inclusive
        tasks = scheduling_request.tasks
        dated_sessions = scheduling_request.dated_sessions
        dated_sessions.sort(key = lambda x : x.date)
        usable_time_config = scheduling_request.usable_time

        response = SchedulingResponse()
        for i in range(config.daily_info_days.value):
            daily_info = DailyInfo()
            daily_info.date = schedule_start.add_days(i)
            response.daily_infos.append(daily_info)

        response.debug = ""

        t = debug_timer.pop()
        response.debug += "setup: {:.3f}s\n".format(t)

        # parse work time config
        debug_timer.push()
        
        usable_time_dict = self.usable_time_parser.get_usable_time_dict(schedule_start, schedule_end, usable_time_config)
        
        for daily_info in response.daily_infos:
            daily_info.usable_time = usable_time_dict[daily_info.date]

        t = debug_timer.pop()
        response.debug += "parse work time config: {:.3f}s\n".format(t)

        # report bad info tasks
        debug_timer.push()
        
        bad_info_tasks = self.task_filter.get_bad_info_tasks(tasks)
        response.alerts.bad_info_tasks = bad_info_tasks

        t = debug_timer.pop()
        response.debug += "report bad info tasks: {:.3f}s\n".format(t)

        # task preprocessing and overdue tasks
        debug_timer.push()
        
        todo_tasks = self.task_filter.get_todo_tasks(tasks)
        schedulable_tasks = self.task_filter.get_schedulable_tasks(todo_tasks)

        overdue_tasks = self.task_filter.get_overdue_tasks(schedulable_tasks, schedule_start)
        response.alerts.overdue_tasks = overdue_tasks

        schedulable_tasks = self.task_repeater.repeat(schedulable_tasks, schedule_start, schedule_end)
        stress_contributor_tasks_mask = self.task_filter.get_stress_contributor_tasks_mask(schedulable_tasks, schedule_start, schedule_end)
        self.task_filter.assign_implicit_stressless(schedulable_tasks, stress_contributor_tasks_mask)

        t = debug_timer.pop()
        response.debug += "preprocess tasks: {:.3f}s\n".format(t)
        
        # progress count
        debug_timer.push()
        
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
        strong_progress_without_today = self.progress_counter.count(schedulable_tasks, strong_dated_sessions_without_today, sessions_sorted = True)
        strong_progress_today = self.progress_counter.count(schedulable_tasks, strong_dated_sessions_today, sessions_sorted = True)
        strong_progress = strong_progress_without_today.combine(strong_progress_today)

        t = debug_timer.pop()
        response.debug += "progress count: {:.3f}s\n".format(t)

        # report bad estimate tasks
        debug_timer.push()
        
        bad_estimate_tasks = self.task_filter.get_bad_estimate_tasks(schedulable_tasks, strong_progress)
        response.alerts.bad_estimate_tasks = bad_estimate_tasks

        t = debug_timer.pop()
        response.debug += "report bad estimate tasks: {:.3f}s\n".format(t)

        # make schedule objects
        debug_timer.push()
        
        early_schedule = Schedule.from_usable_time_dict(schedule_start, schedule_end, usable_time_dict)
        early_schedule.add_dated_sessions(strong_dated_sessions)
        late_schedule = early_schedule.copy()
        
        for daily_info in response.daily_infos:
            daily_info.actual_usable_time.value = early_schedule.get_usable_time(daily_info.date)

        t = debug_timer.pop()
        response.debug += "parse work time config: {:.3f}s\n".format(t)

        # impossible tasks check
        debug_timer.push()
        
        late_plan_result = self.planner.plan(
            schedulable_tasks,
            late_schedule,
            direction = FillDirection.LATE,
            progress_info = strong_progress,
            tasks_mask = stress_contributor_tasks_mask
        )
        impossible_tasks = late_plan_result.impossible_tasks
        response.alerts.impossible_tasks = impossible_tasks

        t = debug_timer.pop()
        response.debug += "impossible tasks check: {:.3f}s\n".format(t)
        
        # free time info and stress
        debug_timer.push()
        
        stress_info = self.stress_analyzer.analyze(late_schedule)
        response.general.stress.value = stress_info.overall_stress.value
        response.general.extra_time_ratio.value = stress_info.extra_time_ratio.value
        response.general.highest_stress_date = stress_info.highest_stress_date
        highest_stress_task = self.stress_analyzer.get_highest_stress_task(schedulable_tasks, stress_info.highest_stress_date)
        response.general.highest_stress_task = highest_stress_task.id if highest_stress_task is not None else None

        for daily_info in response.daily_infos:
            daily_stress_info = stress_info.daily_stress_infos[daily_info.date]
            daily_info.free_time.value = daily_stress_info.acc_free_time.value
            daily_info.average_stress.value = daily_stress_info.acc_average_stress.value
            daily_info.average_etr.value = daily_stress_info.acc_extra_time_ratio.value

        t = debug_timer.pop()
        response.debug += "analyze free time and stress: {:.3f}s\n".format(t)

        # schedule suggestion for fragmented tasks (today or more)
        debug_timer.push()
        
        # compute fragmented time for today (using maximum free time)
        # have the randomizer seeded with today's day string.
        # select task randomly from all tasks
        fragment_sessions = self.fragmentizer.suggest_fragments(
            todo_tasks,
            late_schedule,
            stress_info,
            config.fragmentation_config
        )

        t = debug_timer.pop()
        response.debug += "fragment tasks: {:.3f}s\n".format(t)

        # additional stress info
        debug_timer.push()
        
        late_schedule_with_optimal = early_schedule.copy()
        self.planner.plan(
            schedulable_tasks,
            late_schedule_with_optimal,
            direction = FillDirection.EARLY,
            progress_info = strong_progress,
            early_stop = schedule_start,
            tasks_mask = stress_contributor_tasks_mask,
            session_weakness = SessionWeaknessEnum.STRONG
        )
        optimal_dated_sessions = [
            DatedSession(schedule_start, session)
            for session in
            late_schedule_with_optimal.get_sessions(schedule_start)
        ]
        strong_progress_with_optimal = self.progress_counter.count(schedulable_tasks, optimal_dated_sessions, sessions_sorted = True)
        strong_progress_with_optimal = strong_progress_with_optimal.combine(strong_progress_without_today)
        self.planner.plan(
            schedulable_tasks,
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
            schedulable_tasks,
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
        strong_progress_with_suggested = self.progress_counter.count(schedulable_tasks, suggested_dated_sessions, sessions_sorted = True)
        strong_progress_with_suggested = strong_progress_with_suggested.combine(strong_progress_without_today)
        self.planner.plan(
            schedulable_tasks,
            late_schedule_with_suggested,
            direction = FillDirection.LATE,
            progress_info = strong_progress_with_suggested,
            tasks_mask = stress_contributor_tasks_mask
        )
        
        stress_info_with_optimal = self.stress_analyzer.analyze(late_schedule_with_optimal)
        response.general.stress_with_optimal.value = stress_info_with_optimal.overall_stress.value
        response.general.etr_with_optimal.value = stress_info_with_optimal.extra_time_ratio.value
        
        stress_info_with_suggested = self.stress_analyzer.analyze(late_schedule_with_suggested)
        response.general.stress_with_suggested.value = stress_info_with_suggested.overall_stress.value
        response.general.etr_with_suggested.value = stress_info_with_suggested.extra_time_ratio.value
        
        stress_info_without_today = self.stress_analyzer.analyze(late_schedule, bias = late_schedule.get_usable_time(schedule_start))
        response.general.stress_without_today.value = stress_info_without_today.overall_stress.value
        response.general.etr_without_today.value = stress_info_without_today.extra_time_ratio.value

        t = debug_timer.pop()
        response.debug += "additional stress info: {:.3f}s\n".format(t)

        # schedule suggestion for deadline tasks
        debug_timer.push()
        
        # run forward pass (while accounting for today's fragmented time) to generate suggestion
        strong_progress_with_fragments = self.progress_counter.count(schedulable_tasks, fragment_sessions, sessions_sorted = True)
        strong_progress_with_fragments = strong_progress_with_fragments.combine(strong_progress)
        early_schedule.add_dated_sessions(fragment_sessions)
        self.planner.plan(
            schedulable_tasks,
            early_schedule,
            direction = FillDirection.EARLY,
            progress_info = strong_progress_with_fragments,
            early_stop = daily_info_end
        )

        t = debug_timer.pop()
        response.debug += "compute suggested schedule: {:.3f}s\n".format(t)
        
        # compute task stress of each session
        debug_timer.push()
        
        # TODO

        for daily_info in response.daily_infos:
            sessions = early_schedule.get_sessions(daily_info.date)
            self.stress_analyzer.compute_session_extra_info(daily_info.date, sessions, schedulable_tasks)
            daily_info.sessions = sessions

        t = debug_timer.pop()
        response.debug += "session extra info: {:.3f}s\n".format(t)

        t = debug_timer.pop()
        response.debug += "total: {:.3f}s\n".format(t)

        if not config.show_debug_messages.value:
            response.debug = None

        return response
    

def main():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    main()
