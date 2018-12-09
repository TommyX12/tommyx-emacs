from enum import Enum

from data_structure import *
import util


class TasksPlanningData(object):
    def __init__(self, tasks):
        self.amounts = {
            task.id.value: task.amount.value - task.done.value
            for task in tasks
        }

    def is_task_done(self, task_id):
        return self.amounts[task_id] <= 0

    def get_amount_left(self, task_id):
        return self.amounts[task_id]

    def decrease_amount(self, task_id, amount):
        if amount > self.amounts[task_id]:
            raise ValueError()

        self.amounts[task_id] -= amount


class ScheduleFiller(object):
    def __init__(self, schedule, direction):
        self.schedule = schedule

        self.delta = None
        self.get_schedule_date = None
        self.is_before = None

        if direction == FillDirection.EARLY:
            self.delta = 1
            self.get_schedule_date = ScheduleFiller._get_schedule_date_early
            self.is_before = ScheduleFiller._is_before_early

        elif direction == FillDirection.LATE:
            self.delta = -1
            self.get_schedule_date = ScheduleFiller._get_schedule_date_late
            self.is_before = ScheduleFiller._is_before_late

        else:
            raise ValueError()

    def _get_schedule_date_early(date):
        return date

    def _get_schedule_date_late(date):
        return date.add_days(-1)

    def fill(self, task_id, amount, date_from, date_to):
        '''
        Fill task_id with amount from 00:00 of date_from to 00:00 of date_to.
        The concept of "before" is determined by fill direction,
        which means date_from should be later than date_to if direction is reversed.
        '''
        if self.is_before(date_to, date_from):
            raise ValueError()

        date = date_from
        amount_filled = 0
        
        # while date < date_to
        while self.is_before(date, date_to):
            if amount <= 0:
                break

            schedule_date = self.get_schedule_date(date)
            free_time = self.schedule.get_free_time(schedule_date)
            session_amount = min(amount, free_time)
            if session_amount > 0:
                amount -= session_amount
                amount_filled += session_amount
                session = Session()
                session.id.value = task_id
                session.amount.value = session_amount
                session.type.value = SessionTypeEnum.TASK
                self.schedule.add_session(schedule_date, session)

            date = date.add_days(self.delta)

        return amount_filled

    def _is_before_early(date1, date2):
        return date1 < date2
    
    def _is_before_late(date1, date2):
        return date1 > date2
    

class GreedySchedulingQueue(object):
    '''
    A priority queue for greedy scheduling.
    Data must be unique and hashable.
    '''

    def __init__(self, descending = False):
        self.heap = []
        self.indices = {}
        self.comp = None
        if descending:
            self.comp = GreedySchedulingQueue._comp_descending

        else:
            self.comp = GreedySchedulingQueue._comp_ascending

    def _comp_ascending(a, b):
        return a < b

    def _comp_descending(a, b):
        return b < a
    
    def clear(self):
        '''
        Clears the queue.
        '''
        
        self.heap = []
        self.indices = {}

    def is_empty(self):
        return len(self.heap) == 0

    def top(self):
        '''
        Returns the data in the queue with the highest priority.
        '''
        return self.heap[0][0]

    def add(self, data, priority):
        '''
        Add data with given priority.
        '''
        if data in self.indices:
            raise ValueError()

        self.heap.append((data, priority))
        self.indices[data] = len(self.heap) - 1
        self._float_up(len(self.heap) - 1)
    
    def delete(self, data):
        if data not in self.indices:
            return
            
        index = self.indices[data]
        old_priority = self.heap[index][1]
        new_data, priority = self.heap[len(self.heap) - 1]
        self.indices[new_data] = index
        self.heap[index] = (new_data, priority)

        self.heap.pop()
        del self.indices[data]

        # if priority > old_priority:
        if self.comp(old_priority, priority):
            self._float_up(index)

        else:
            self._float_down(index)
    
    def _float_down(self, i):
        '''
        Pushes the i-th entry downward on the heap when necessary.
        '''
        
        i_value, i_priority = self.heap[i]
        j = i
        
        while j < len(self.heap) - 1:
            l = 2 * i + 1
            r = 2 * i + 2
            
            # if l < len(self.data) and self.data[l][1] > self.data[j][1]:
            if l < len(self.heap) and self.comp(self.heap[j][1], self.heap[l][1]):
                j = l
                
            # if r < len(self.data) and self.data[r][1] > self.data[j][1]:
            if r < len(self.heap) and self.comp(self.heap[j][1], self.heap[r][1]):
                j = r
            
            if j == i:
                break
            
            self.indices[self.heap[j][0]] = i
            self.indices[i_value] = j
            self.heap[i], self.heap[j] = self.heap[j], self.heap[i]
            
            i = j
        
    def _float_up(self, i):
        '''
        Pushes the i-th entry upward on the heap when necessary.
        '''
        i_value, i_priority = self.heap[i]
        j = i
        
        while j > 0:
            j = (i - 1) // 2
            
            # if i_priority <= self.data[j][1]:
            if not self.comp(self.heap[j][1], i_priority):
                break
            
            self.indices[self.heap[j][0]] = i
            self.indices[i_value] = j
            self.heap[i], self.heap[j] = self.heap[j], self.heap[i]
            
            i = j
        

class DateIterator(object):
    
    def __init__(self, start, end, direction):
        if start > end:
            raise ValueError()

        # The next date to return.
        self.date = None
        self.delta = None
        self.start = start
        self.end = end
        self.has_next = None
        
        if direction == FillDirection.EARLY:
            self.date = start
            self.delta = 1
            self.has_next = self._has_next_early

        elif direction == FillDirection.LATE:
            self.date = end
            self.delta = -1
            self.has_next = self._has_next_late

        else:
            raise ValueError()

    def get_next(self):
        '''
        Return next value, but do not increment.
        '''
        return self.date

    def next(self):
        if not self.has_next():
            return None # Can be optimized.

        date = self.date
        self.date = self.date.add_days(self.delta)
        return date

    def _has_next_early(self):
        return self.date <= self.end

    def _has_next_late(self):
        return self.date >= self.start


class TaskEventType(Enum):
    TASK_START = 0
    TASK_END = 1


class TaskEvent(object):
    '''
    Describes a point in time (00:00 of self.date) where an event occur for a task.
    Can either be TaskEventType.TASK_START or TaskEventType.TASK_END.
    
    Note that TASK_START may be the deadline of task if scheduling backward.
    '''

    def __init__(self, task_id, date, event_type):
        self.task_id = task_id
        self.date = date
        self.event_type = event_type

    def __lt__(self, other):
        return self.date < other.date


class TaskEventsIterator(object):

    def __init__(self, tasks, start, end, direction):
        '''
        TODO
        Start and end represents 00:00 of that day.
        '''
        if start > end:
            raise ValueError()

        self.task_events = []

        self.direction = direction

        # Points to next event to be read.
        self.index = 0
        
        for task in tasks:
            if task.start > task.end:
                raise ValueError()

            self.task_events.append(TaskEvent(
                task.id.value,
                util.clamp(task.start, start, end),
                TaskEventType.TASK_START
            ))
            self.task_events.append(TaskEvent(
                task.id.value,
                util.clamp(task.end.add_days(1), start, end),
                TaskEventType.TASK_END
            ))

        if direction == FillDirection.EARLY:
            self.task_events.sort(key = lambda x: x.date)
            self.is_before = TaskEventsIterator._is_before_early

        elif direction == FillDirection.LATE:
            self.task_events.sort(key = lambda x: x.date, reverse = True)
            self.is_before = TaskEventsIterator._is_before_late

        else:
            raise ValueError()

    def read_event_to(self, date):
        '''
        Return the next unread event before and including at 00:00 of date.
        The concept of "before" is determined by fill direction.
        '''

        if self.index < 0 or self.index >= len(self.task_events):
            return None

        next_event = self.task_events[self.index]
        # if next_event.date <= date
        if not self.is_before(date, next_event.date):
            self.index += 1
            return next_event

        return None

    def _is_before_early(date1, date2):
        return date1 < date2
    
    def _is_before_late(date1, date2):
        return date1 > date2
    

class Planner(object):
    '''
    TODO

    TODO
    '''

    def __init__(self):
        pass

    def plan(self, tasks, schedule, direction = FillDirection.EARLY):
        '''
        TODO
        Mutates schedule to fill sessions in.
        '''
        result = PlannerResult()
        
        tasks_data = TasksPlanningData(tasks)
        
        # These days represent 00:00 of that day.
        planning_start = schedule.get_schedule_start()
        planning_end = schedule.get_schedule_end().add_days(1)

        task_events_iterator = TaskEventsIterator(tasks, planning_start, planning_end, direction)
        filler = ScheduleFiller(schedule, direction)
        date_iterator = DateIterator(planning_start, planning_end, direction)
        queue = GreedySchedulingQueue(descending = (direction == FillDirection.LATE))

        while date_iterator.has_next():
            date = date_iterator.next()

            # Read events.
            while True:
                next_event = task_events_iterator.read_event_to(date)
                if next_event is None:
                    break

                if next_event.event_type == TaskEventType.TASK_START:
                    queue.add(next_event.task_id, next_event.date)

                elif next_event.event_type == TaskEventType.TASK_END:
                    # Check if task is done.
                    if not tasks_data.is_task_done(next_event.task_id):
                        impossible_task = ImpossibleTask()
                        impossible_task.id.value = next_event.task_id
                        result.impossible_tasks.append(impossible_task)

                    queue.delete(next_event.task_id)

            if not date_iterator.has_next():
                # At last day.
                break

            next_date = date_iterator.get_next()

            # Fill sessions.
            while True:
                if queue.is_empty():
                    break

                next_task_id = queue.top()
                amount_left = tasks_data.get_amount_left(next_task_id)
                if amount_left < 0:
                    raise ValueError()

                # Fill session, until 00:00 of next_date
                amount_filled = filler.fill(next_task_id, amount_left, date, next_date)
                tasks_data.decrease_amount(next_task_id, amount_filled)
                
                # No more free time.
                if amount_filled < amount_left:
                    break

                # Remaining time depleted (amount_filled == amount_left).
                queue.delete(next_task_id)
        
        return result


def main():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    main()

