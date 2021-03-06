import unittest
from usable_time_parser import *
from data_structure import *
from scheduling_util import *
import test_util
test_util.allow_parent_import()


class SchedulingUtilTest(unittest.TestCase):
    '''
    TODO: Test more edge cases.
    TODO: Test infinite deadline.
    TODO: Test performance.
    '''

    def test_task_index_finder(self):
        t = TaskIndexFinder()
        t.add(1, 2)
        t.add(1, 2)
        t.add(1, 3)
        t.add(2, 8)
        t.add(2, 2)
        t.add(2, 9)
        t.add(3, 1)
        t.add(3, 1)
        t.add(3, 1)
        t.remove(3, 1)
        t.remove(2, 9)
        self.assertEqual(sorted(list(t.get_task_indices(1))), [2, 3])
        self.assertEqual(sorted(list(t.get_task_indices(2))), [2, 8])
        self.assertEqual(sorted(list(t.get_task_indices(3))), [])

    def test_schedule_filler(self):
        schedule_start = Date().decode_self('2018-12-01')
        schedule_end = Date().decode_self('2019-01-01')
        usable_time_config = [UsableTimeConfigEntry().decode_self(
            {'selector': 'default', 'duration': 20})]
        usable_time_dict = UsableTimeParser().get_usable_time_dict(
            schedule_start, schedule_end, usable_time_config)
        schedule = Schedule.from_usable_time_dict(
            schedule_start, schedule_end, usable_time_dict)
        s = ScheduleFiller(schedule, FillDirection.EARLY)
        self.assertEqual(s.fill(1, 15, Date().decode_self(
            '2018-12-04'), Date().decode_self('2018-12-06')), 15)
        self.assertEqual(s.fill(2, 20, Date().decode_self(
            '2018-12-04'), Date().decode_self('2018-12-06')), 20)
        self.assertEqual(s.fill(3, 10, Date().decode_self(
            '2018-12-05'), Date().decode_self('2018-12-06')), 5)
        schedule = Schedule.from_usable_time_dict(
            schedule_start, schedule_end, usable_time_dict)
        s = ScheduleFiller(schedule, FillDirection.LATE)
        self.assertEqual(s.fill(1, 15, Date().decode_self(
            '2018-12-06'), Date().decode_self('2018-12-04')), 15)
        self.assertEqual(s.fill(2, 20, Date().decode_self(
            '2018-12-06'), Date().decode_self('2018-12-04')), 20)
        self.assertEqual(s.fill(3, 10, Date().decode_self(
            '2018-12-05'), Date().decode_self('2018-12-04')), 5)

    def test_greedy_scheduling_queue(self):
        g = GreedySchedulingQueue(descending=False)
        g.add(1, 4)
        g.add(2, 1)
        self.assertEqual(g.top(), 1)
        g.add(3, 2)
        g.add(4, 8)
        self.assertEqual(g.top(), 4)
        self.assertEqual(g.is_empty(), False)
        g.add(5, 6)
        g.add(6, 3)
        self.assertEqual(g.top(), 4)
        g.delete(4)
        self.assertEqual(g.top(), 5)
        g.clear()
        self.assertEqual(g.is_empty(), True)
        g = GreedySchedulingQueue(descending=True)
        g.add(1, 4)
        g.add(2, 2)
        self.assertEqual(g.top(), 2)
        g.add(3, 1)
        g.add(4, 8)
        self.assertEqual(g.top(), 3)
        self.assertEqual(g.is_empty(), False)
        g.add(5, 6)
        g.add(6, 3)
        self.assertEqual(g.top(), 3)
        g.delete(3)
        self.assertEqual(g.top(), 2)
        g.clear()
        self.assertEqual(g.is_empty(), True)

        g.add(1, 1)
        g.delete(1)
        self.assertEqual(g.is_empty(), True)

    def test_date_iterator(self):
        start = Date().decode_self('2018-12-01')
        end = Date().decode_self('2018-12-03')
        d = DateIterator(start, end, FillDirection.EARLY)
        self.assertEqual(d.has_next(), True)
        self.assertEqual(d.get_next().encode(), '2018-12-1')
        self.assertEqual(d.next().encode(), '2018-12-1')
        self.assertEqual(d.next().encode(), '2018-12-2')
        self.assertEqual(d.next().encode(), '2018-12-3')
        self.assertEqual(d.has_next(), False)
        d = DateIterator(start, end, FillDirection.LATE)
        self.assertEqual(d.has_next(), True)
        self.assertEqual(d.get_next().encode(), '2018-12-3')
        self.assertEqual(d.next().encode(), '2018-12-3')
        self.assertEqual(d.next().encode(), '2018-12-2')
        self.assertEqual(d.next().encode(), '2018-12-1')
        self.assertEqual(d.has_next(), False)

    def test_task_events_iterator(self):
        task1 = Task()
        task1.id.value = 1
        task1.start = Date().decode_self('2018-12-01')
        task1.end = Date().decode_self('2018-12-05')
        task2 = Task()
        task2.id.value = 2
        task2.start = Date().decode_self('2018-12-01')
        task2.end = Date().decode_self('2018-12-04')
        task3 = Task()
        task3.id.value = 3
        task3.start = Date().decode_self('2018-12-02')
        task3.end = Date().decode_self('2018-12-05')
        task4 = Task()
        task4.id.value = 4
        task4.start = Date().decode_self('2018-11-15')
        task4.end = Date().decode_self('2018-12-10')
        tasks = [task1, task2, task3, task4]
        start = Date().decode_self('2018-12-01')
        end = Date().decode_self('2018-12-06')
        def p(x): return None if x is None else (str(x.task_id) + ' ' +
                                                 ('s' if x.event_type == TaskEventType.TASK_START else 'e'))
        t = TaskEventsIterator(tasks, start, end, FillDirection.EARLY)
        self.assertEqual(t.read_event_to(
            Date().decode_self('2018-11-01')), None)
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-01'))), '1 s')
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-01'))), '2 s')
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-01'))), '4 s')
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-02'))), '3 s')
        self.assertEqual(t.read_event_to(
            Date().decode_self('2018-12-03')), None)
        self.assertEqual(t.read_event_to(
            Date().decode_self('2018-12-04')), None)
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-05'))), '2 e')
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-06'))), '1 e')
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-06'))), '3 e')
        self.assertEqual(t.read_event_to(
            Date().decode_self('2018-12-06')), None)
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-07'))), '4 e')
        t = TaskEventsIterator(tasks, start, end, FillDirection.LATE)
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-07'))), '4 s')
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-06'))), '1 s')
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-06'))), '3 s')
        self.assertEqual(t.read_event_to(
            Date().decode_self('2018-12-06')), None)
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-05'))), '2 s')
        self.assertEqual(t.read_event_to(
            Date().decode_self('2018-12-04')), None)
        self.assertEqual(t.read_event_to(
            Date().decode_self('2018-12-03')), None)
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-02'))), '3 e')
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-01'))), '1 e')
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-01'))), '2 e')
        self.assertEqual(
            p(t.read_event_to(Date().decode_self('2018-12-01'))), '4 e')
        self.assertEqual(t.read_event_to(
            Date().decode_self('2018-11-01')), None)

    def test_sampler(self):
        s = Sampler(0)
        a = [s.sample([1, 2, 3]) for _ in range(100)]
        for _ in range(10):
            s = Sampler(0)
            b = [s.sample([1, 2, 3]) for _ in range(100)]
            self.assertEqual(a, b)


if __name__ == '__main__':
    unittest.main()
