
from data_structure import *
from scheduling_util import *


class ProgressCounter(object):

    def __init__(self):
        pass

    def count(self, tasks, dated_sessions, sessions_sorted = False):
        if not sessions_sorted:
            dated_sessions = sorted(dated_sessions, key = lambda x : x.date)

        task_index_finder = TaskIndexFinder()
        
        acc = ProgressInfo(len(tasks))
        result = ProgressInfo(len(tasks))

        session_index = 0

        task_events_iterator = TaskEventsIterator(tasks, None, None, FillDirection.EARLY)

        while True:
            next_event = task_events_iterator.read_next_event()
            if next_event is None:
                break

            while session_index < len(dated_sessions) and dated_sessions[session_index].date < next_event.date:
                task_id = dated_sessions[session_index].session.id.value
                for task_index in task_index_finder.get_task_indices(task_id):
                    acc.add_done_amount(
                        task_index,
                        dated_sessions[session_index].session.amount.value
                    )
                    
                session_index += 1
                
            if next_event.event_type == TaskEventType.TASK_START:
                result.set_done_amount(
                    next_event.task_index,
                    acc.get_done_amount(next_event.task_index)
                )
                task_index_finder.add(next_event.task_id, next_event.task_index)

            elif next_event.event_type == TaskEventType.TASK_END:
                result.set_done_amount(
                    next_event.task_index,
                    acc.get_done_amount(next_event.task_index) - result.get_done_amount(next_event.task_index)
                )
                task_index_finder.remove(next_event.task_id, next_event.task_index)

        return result


def main():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    main()
