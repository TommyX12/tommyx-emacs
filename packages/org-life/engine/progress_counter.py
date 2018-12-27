
from data_structure import *
from scheduling_util import *


class ProgressCounter(object):

    def __init__(self):
        pass

    def count(self, tasks, dated_sessions, sessions_sorted = False):
        if not sessions_sorted:
            dated_sessions = sorted(dated_sessions, key = lambda x : x.date)

        acc = ProgressInfo()
        result = ProgressInfo()

        session_index = 0

        task_events_iterator = TaskEventsIterator(tasks, None, None, FillDirection.EARLY)

        while True:
            next_event = task_events_iterator.read_next_event()
            if next_event is None:
                break

            while session_index < len(dated_sessions) and dated_sessions[session_index].date < next_event.date:
                acc.add_done_amount(
                    dated_sessions[session_index].session.id.value,
                    dated_sessions[session_index].session.amount.value
                )
                session_index += 1
                
            if next_event.event_type == TaskEventType.TASK_START:
                result.set_done_amount(
                    next_event.task_id,
                    acc.get_done_amount(next_event.task_id)
                )

            elif next_event.event_type == TaskEventType.TASK_END:
                result.set_done_amount(
                    next_event.task_id,
                    acc.get_done_amount(next_event.task_id) - result.get_done_amount(next_event.task_id)
                )

        return result

    
def main():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    main()
