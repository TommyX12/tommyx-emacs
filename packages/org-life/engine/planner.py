import math
from enum import Enum

from data_structure import *
from scheduling_util import *
from heuristics import GreedySchedulingPolicy
import util


class Planner(object):
    
    def __init__(self):
        pass

    def plan(self, tasks, schedule, direction = FillDirection.EARLY, progress_info = None, tasks_mask = None, early_stop = None, session_weakness = SessionWeaknessEnum.WEAK, policy = None, mutate_progress = False):
        '''
        TODO: test tasks_mask
        Mutates schedule to fill sessions in.
        
        Note that planner doesn't account for existing progress automatically.
        In order to do that, supply progress_info.
        '''
        if direction != FillDirection.EARLY:
            early_stop = None

        result = PlannerResult()
        
        if progress_info is None:
            progress_info = ProgressInfo(tasks)

        elif not mutate_progress:
            progress_info = progress_info.copy()

        result.progress_info = progress_info
        
        # These days represent 00:00 of that day.
        planning_start = schedule.get_schedule_start()
        planning_end = schedule.get_schedule_end().add_days(1)

        task_events_iterator = TaskEventsIterator(tasks, planning_start, planning_end, direction, tasks_mask)
        filler = ScheduleFiller(schedule, direction)
            
        date_iterator = DateIterator(planning_start, planning_end, direction)
        if policy is None:
            policy = GreedySchedulingPolicy()

        policy.initialize(direction, tasks, progress_info, schedule)

        if direction == FillDirection.EARLY:
            get_schedule_date = Planner._get_schedule_date_early

        else:
            get_schedule_date = Planner._get_schedule_date_late

        while date_iterator.has_next():
            date = date_iterator.next()

            # Read events.
            while True:
                next_event = task_events_iterator.read_event_to(date)
                if next_event is None:
                    break

                if next_event.event_type == TaskEventType.TASK_START:
                    policy.add(next_event.task_index, tasks[next_event.task_index], next_event)

                elif next_event.event_type == TaskEventType.TASK_END:
                    # Check if task is done.
                    if not progress_info.is_task_done(next_event.task_index):
                        impossible_task = ImpossibleTask()
                        impossible_task.id.value = next_event.task_id
                        impossible_task.amount.value = progress_info.get_amount_left(next_event.task_index)
                        result.impossible_tasks.append(impossible_task)

                        session = Session()
                        session.id.value = next_event.task_id
                        session.amount.value = progress_info.get_amount_left(next_event.task_index)
                        session.type.value = SessionTypeEnum.OVERLIMIT
                        session.weakness.value = session_weakness
                        session.task_index = TaskIndex(next_event.task_index)
                        schedule.add_session(
                            get_schedule_date(date),
                            session
                        )

                    policy.delete(next_event.task_index)

            if not date_iterator.has_next():
                # At last day.
                break

            next_date = date_iterator.get_next()

            # Fill sessions.
            while True:
                next_task_index = policy.get_next(date)
                if next_task_index is None:
                    break

                next_task_id = tasks[next_task_index].id.value
                amount_left = progress_info.get_amount_left(next_task_index)
                max_session_duration = min(amount_left, policy.get_max_session_duration())

                # Fill session, until 00:00 of next_date
                amount_filled = filler.fill(
                    next_task_id,
                    max_session_duration,
                    date,
                    next_date,
                    session_weakness,
                    SessionTypeEnum.TASK
                    if not tasks[next_task_index].stressless.value
                    else SessionTypeEnum.FRAGMENT,
                    next_task_index,
                    amount_left
                )
                progress_info.decrease_amount(next_task_index, amount_filled)

                policy.update(next_task_index, date, next_date, amount_filled)
                
                # No more free time.
                if amount_filled < max_session_duration:
                    break

                # Remaining time depleted.
                if amount_filled == amount_left:
                    policy.delete(next_task_index)

            if early_stop is not None and date >= early_stop:
                break
        
        return result

    def _get_schedule_date_early(date):
        return date.add_days(-1)

    def _get_schedule_date_late(date):
        return date


def main():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    main()

