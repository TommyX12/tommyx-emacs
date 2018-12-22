import test_util
test_util.allow_parent_import()

from planner import *
from scheduling_util import *
from data_structure import *
from usable_time_parser import *

import unittest

class PlannerTest(unittest.TestCase):
    '''
    TODO: Test more edge cases.
    TODO: Test infinite deadline.
    TODO: Test performance.
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
        usable_time_config = [UsableTimeConfigEntry().decode_self({'selector':'default','duration':20})]
        usable_time_dict = UsableTimeParser().get_usable_time_dict(schedule_start, schedule_end, usable_time_config)
        schedule = Schedule.from_usable_time_dict(schedule_start, schedule_end, usable_time_dict)

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
        
        schedule = Schedule.from_usable_time_dict(schedule_start, schedule_end, usable_time_dict)
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
        
        schedule = Schedule.from_usable_time_dict(schedule_start, schedule_end, usable_time_dict)
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
        
        schedule = Schedule.from_usable_time_dict(schedule_start, schedule_end, usable_time_dict)
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
