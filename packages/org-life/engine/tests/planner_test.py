import test_util
test_util.allow_parent_import()

from planner import *
from data_structure import *
from work_time_parser import *

import unittest

class PlannerTest(unittest.TestCase):
    '''
    TODO: Test more edge cases.
    TODO: Test infinite deadline.
    '''

    def test_tasks_planning_data(self):
        task1 = Task()
        task1.id.value = 1
        task1.amount.value = 50
        task1.done.value = 0
        task2 = Task()
        task2.id.value = 2
        task2.amount.value = 80
        task2.done.value = 40
        tasks = [task1, task2]
        t = TasksPlanningData(tasks)
        self.assertEqual(t.get_amount_left(1), 50)
        self.assertEqual(t.get_amount_left(2), 40)
        t.decrease_amount(1, 50)
        t.decrease_amount(2, 20)
        self.assertEqual(t.get_amount_left(1), 0)
        self.assertEqual(t.get_amount_left(2), 20)
        self.assertEqual(t.is_task_done(1), True)
        self.assertEqual(t.is_task_done(2), False)

    def test_schedule_filler(self):
        schedule_start = Date().decode_self('2018-12-01')
        schedule_end = Date().decode_self('2019-01-01')
        work_time_config = [WorkTimeConfigEntry().decode_self({'selector':'default','duration':20})]
        work_time_dict = WorkTimeParser().get_work_time_dict(schedule_start, schedule_end, work_time_config)
        schedule = Schedule.from_work_time_dict(schedule_start, schedule_end, work_time_dict)
        s = ScheduleFiller(schedule, FillDirection.EARLY)
        self.assertEqual(s.fill(1, 15, Date().decode_self('2018-12-04'), Date().decode_self('2018-12-06')), 15)
        self.assertEqual(s.fill(2, 20, Date().decode_self('2018-12-04'), Date().decode_self('2018-12-06')), 20)
        self.assertEqual(s.fill(3, 10, Date().decode_self('2018-12-05'), Date().decode_self('2018-12-06')), 5)
        schedule = Schedule.from_work_time_dict(schedule_start, schedule_end, work_time_dict)
        s = ScheduleFiller(schedule, FillDirection.LATE)
        self.assertEqual(s.fill(1, 15, Date().decode_self('2018-12-06'), Date().decode_self('2018-12-04')), 15)
        self.assertEqual(s.fill(2, 20, Date().decode_self('2018-12-06'), Date().decode_self('2018-12-04')), 20)
        self.assertEqual(s.fill(3, 10, Date().decode_self('2018-12-05'), Date().decode_self('2018-12-04')), 5)

    def test_greedy_scheduling_queue(self):
        g = GreedySchedulingQueue(descending = False)
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
        g = GreedySchedulingQueue(descending = True)
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
        tasks = [task1, task2, task3]
        start = Date().decode_self('2018-12-01')
        end = Date().decode_self('2018-12-06')
        p = lambda x: None if x is None else (str(x.task_id) + ' ' + ('s' if x.event_type == TaskEventType.TASK_START else 'e'))
        t = TaskEventsIterator(tasks, start, end, FillDirection.EARLY)
        self.assertEqual(t.read_event_to(Date().decode_self('2018-11-01')), None)
        self.assertEqual(p(t.read_event_to(Date().decode_self('2018-12-01'))), '1 s')
        self.assertEqual(p(t.read_event_to(Date().decode_self('2018-12-01'))), '2 s')
        self.assertEqual(p(t.read_event_to(Date().decode_self('2018-12-02'))), '3 s')
        self.assertEqual(t.read_event_to(Date().decode_self('2018-12-03')), None)
        self.assertEqual(t.read_event_to(Date().decode_self('2018-12-04')), None)
        self.assertEqual(p(t.read_event_to(Date().decode_self('2018-12-05'))), '2 e')
        self.assertEqual(p(t.read_event_to(Date().decode_self('2018-12-06'))), '1 e')
        self.assertEqual(p(t.read_event_to(Date().decode_self('2018-12-06'))), '3 e')
        self.assertEqual(t.read_event_to(Date().decode_self('2018-12-07')), None)
        t = TaskEventsIterator(tasks, start, end, FillDirection.LATE)
        self.assertEqual(t.read_event_to(Date().decode_self('2018-12-07')), None)
        self.assertEqual(p(t.read_event_to(Date().decode_self('2018-12-06'))), '1 s')
        self.assertEqual(p(t.read_event_to(Date().decode_self('2018-12-06'))), '3 s')
        self.assertEqual(p(t.read_event_to(Date().decode_self('2018-12-05'))), '2 s')
        self.assertEqual(t.read_event_to(Date().decode_self('2018-12-04')), None)
        self.assertEqual(t.read_event_to(Date().decode_self('2018-12-03')), None)
        self.assertEqual(p(t.read_event_to(Date().decode_self('2018-12-02'))), '3 e')
        self.assertEqual(p(t.read_event_to(Date().decode_self('2018-12-01'))), '1 e')
        self.assertEqual(p(t.read_event_to(Date().decode_self('2018-12-01'))), '2 e')
        self.assertEqual(t.read_event_to(Date().decode_self('2018-11-01')), None)

    def test_planner(self):
        task1 = Task()
        task1.id.value = 1
        task1.start = Date().decode_self('2018-12-01')
        task1.end = Date().decode_self('2018-12-05')
        task1.amount.value = 10
        task1.done.value = 0
        task2 = Task()
        task2.id.value = 2
        task2.start = Date().decode_self('2018-12-01')
        task2.end = Date().decode_self('2018-12-04')
        task2.amount.value = 25
        task2.done.value = 5
        task3 = Task()
        task3.id.value = 3
        task3.start = Date().decode_self('2018-12-02')
        task3.end = Date().decode_self('2018-12-05')
        task3.amount.value = 55
        task3.done.value = 20
        task4 = Task()
        task4.id.value = 4
        task4.start = Date().decode_self('2018-12-02')
        task4.end = Date().decode_self('2018-12-05')
        task4.amount.value = 20
        task4.done.value = 20
        tasks = [task1, task2, task3, task4]

        schedule_start = Date().decode_self('2018-11-30')
        schedule_end = Date().decode_self('2018-12-06')
        work_time_config = [WorkTimeConfigEntry().decode_self({'selector':'default','duration':20})]
        work_time_dict = WorkTimeParser().get_work_time_dict(schedule_start, schedule_end, work_time_config)
        schedule = Schedule.from_work_time_dict(schedule_start, schedule_end, work_time_dict)

        p = Planner()
        result = p.plan(tasks, schedule, direction = FillDirection.EARLY)
        impossible_tasks = result.impossible_tasks
        self.assertEqual(str(schedule), '''==========
2018-11-30: [20]
2018-12-1: [0]
- 2: 20
2018-12-2: [0]
- 1: 10
- 3: 10
2018-12-3: [0]
- 3: 20
2018-12-4: [15]
- 3: 5
2018-12-5: [20]
2018-12-6: [20]
==========
''')
        self.assertEqual(len(impossible_tasks), 0)
        
        schedule = Schedule.from_work_time_dict(schedule_start, schedule_end, work_time_dict)
        result = p.plan(tasks, schedule, direction = FillDirection.LATE)
        impossible_tasks = result.impossible_tasks
        self.assertEqual(str(schedule), '''==========
2018-11-30: [20]
2018-12-1: [20]
2018-12-2: [15]
- 1: 5
2018-12-3: [0]
- 2: 15
- 1: 5
2018-12-4: [0]
- 3: 15
- 2: 5
2018-12-5: [0]
- 3: 20
2018-12-6: [20]
==========
''')
        self.assertEqual(len(impossible_tasks), 0)
        
        task1.amount.value = 1000
        task1.done.value = 100
        
        schedule = Schedule.from_work_time_dict(schedule_start, schedule_end, work_time_dict)
        result = p.plan(tasks, schedule, direction = FillDirection.LATE)
        impossible_tasks = result.impossible_tasks
        self.assertEqual(str(schedule), '''==========
2018-11-30: [20]
2018-12-1: [0]
- 1: 20
2018-12-2: [0]
- 1: 20
2018-12-3: [0]
- 2: 15
- 1: 5
2018-12-4: [0]
- 3: 15
- 2: 5
2018-12-5: [0]
- 3: 20
2018-12-6: [20]
==========
''')
        self.assertEqual(len(impossible_tasks), 1)
        self.assertEqual(impossible_tasks[0].id.value, 1)
        self.assertEqual(impossible_tasks[0].amount.value, 855)
        
        schedule = Schedule.from_work_time_dict(schedule_start, schedule_end, work_time_dict)
        result = p.plan(tasks, schedule, direction = FillDirection.EARLY)
        impossible_tasks = result.impossible_tasks
        self.assertEqual(str(schedule), '''==========
2018-11-30: [20]
2018-12-1: [0]
- 2: 20
2018-12-2: [0]
- 1: 20
2018-12-3: [0]
- 1: 20
2018-12-4: [0]
- 1: 20
2018-12-5: [0]
- 1: 20
2018-12-6: [20]
==========
''')
        self.assertEqual(len(impossible_tasks), 2)
        self.assertEqual(impossible_tasks[0].id.value, 1)
        self.assertEqual(impossible_tasks[0].amount.value, 820)
        self.assertEqual(impossible_tasks[1].id.value, 3)
        self.assertEqual(impossible_tasks[1].amount.value, 35)


if __name__ == '__main__':
    unittest.main()
