import test_util
test_util.allow_parent_import()

from task_filter import *
from scheduling_util import *
from data_structure import *
from work_time_parser import *

import unittest

class TaskFilterTest(unittest.TestCase):

    def test_task_filter(self):
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
        tasks = [task1, task2, task3]

        schedule_start = Date().decode_self('2018-12-02')
        schedule_end = Date().decode_self('2018-12-04')

        t = TaskFilter()
        r = t.get_stress_contributor_tasks(tasks, schedule_start, schedule_end)
        self.assertEqual(r, [task2])


if __name__ == '__main__':
    unittest.main()
