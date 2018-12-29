from enum import Enum

from data_structure import *
from scheduling_util import *
import util


class TasksPlanningData(object):
    def __init__(self, tasks, progress_info = None):
        self.amounts = [task.amount.value - task.done.value for task in tasks]

        if progress_info is not None:
            for i in range(len(self.amounts)):
                self.amounts[i] = max(0, self.amounts[i] - progress_info.get_done_amount(i))

    def is_task_done(self, task_index):
        return self.amounts[task_index] <= 0

    def get_amount_left(self, task_index):
        return self.amounts[task_index]

    def decrease_amount(self, task_index, amount):
        self.amounts[task_index] = max(0, self.amounts[task_index] - amount)


class Planner(object):
    
    def __init__(self):
        pass

    def plan(self, tasks, schedule, direction = FillDirection.EARLY, progress_info = None, tasks_mask = None, early_stop = None, session_weakness = SessionWeaknessEnum.WEAK):
        '''
        TODO: test tasks_mask
        Mutates schedule to fill sessions in.
        
        Note that planner doesn't account for existing progress automatically.
        In order to do that, supply progress_info.
        '''
        if direction != FillDirection.EARLY:
            early_stop = None

        result = PlannerResult()
        
        tasks_data = TasksPlanningData(tasks, progress_info)
        
        # These days represent 00:00 of that day.
        planning_start = schedule.get_schedule_start()
        planning_end = schedule.get_schedule_end().add_days(1)

        task_events_iterator = TaskEventsIterator(tasks, planning_start, planning_end, direction, tasks_mask)
        filler = ScheduleFiller(schedule, direction)
        date_iterator = DateIterator(planning_start, planning_end, direction)
        queue = GreedySchedulingQueue(descending = (direction == FillDirection.EARLY))

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
                    queue.add((next_event.task_index, next_event.task_id), next_event.opposite_date)

                elif next_event.event_type == TaskEventType.TASK_END:
                    # Check if task is done.
                    if not tasks_data.is_task_done(next_event.task_index):
                        impossible_task = ImpossibleTask()
                        impossible_task.id.value = next_event.task_id
                        impossible_task.amount.value = tasks_data.get_amount_left(next_event.task_index)
                        result.impossible_tasks.append(impossible_task)

                        session = Session()
                        session.id.value = next_event.task_id
                        session.amount.value = tasks_data.get_amount_left(next_event.task_index)
                        session.type.value = SessionTypeEnum.OVERLIMIT
                        session.weakness.value = session_weakness
                        schedule.add_session(
                            get_schedule_date(date),
                            session
                        )

                    queue.delete((next_event.task_index, next_event.task_id))

            if not date_iterator.has_next():
                # At last day.
                break

            next_date = date_iterator.get_next()

            # Fill sessions.
            while True:
                if queue.is_empty():
                    break

                next_task_index, next_task_id = queue.top()
                amount_left = tasks_data.get_amount_left(next_task_index)

                # Fill session, until 00:00 of next_date
                amount_filled = filler.fill(next_task_id, amount_left, date, next_date, session_weakness)
                tasks_data.decrease_amount(next_task_index, amount_filled)
                
                # No more free time.
                if amount_filled < amount_left:
                    break

                # Remaining time depleted (amount_filled == amount_left).
                queue.delete((next_task_index, next_task_id))

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

