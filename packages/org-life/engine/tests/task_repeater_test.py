import test_util
test_util.allow_parent_import()

from task_repeater import *
from data_structure import *

import unittest

class TaskRepeaterTest(unittest.TestCase):
    '''
    TODO: Test more
    '''

    def test_task_repeater(self):
        self.maxDiff = None
        
        task1 = Task()
        task1.id.value = 1
        task1.start = Date().decode_self('2017-12-01')
        task1.end = Date().decode_self('2018-12-05')
        task1.repeat = TaskRepeat()
        task1.repeat.unit.value = TaskRepeatUnitEnum.DAY
        task1.repeat.value.value = 10
        
        task2 = Task()
        task2.id.value = 2
        task2.start = Date().decode_self('2018-12-01')
        task2.end = Date().decode_self('2018-12-31')
        task2.repeat = TaskRepeat()
        task2.repeat.unit.value = TaskRepeatUnitEnum.MONTH
        task2.repeat.value.value = 1
        
        task3 = Task()
        task3.id.value = 3
        task3.start = Date().decode_self('2018-12-01')
        task3.end = Date().decode_self('2018-12-31')
        
        tasks = [task1, task2, task3]

        schedule_start = Date().decode_self('2018-12-01')
        schedule_end = Date().decode_self('2019-02-01')
        
        t = TaskRepeater()
        r = t.repeat(tasks, schedule_start, schedule_end)
        self.assertEqual(
            [
                (task.id.value, task.start.encode(), task.end.encode())
                for task in r
            ],
            [(1, '2017-12-1', '2018-12-5'),
             (1, '2018-12-6', '2018-12-15'),
             (1, '2018-12-16', '2018-12-25'),
             (1, '2018-12-26', '2019-1-4'),
             (1, '2019-1-5', '2019-1-14'),
             (1, '2019-1-15', '2019-1-24'),
             (1, '2019-1-25', '2019-2-3'),
             (2, '2018-12-1', '2018-12-31'),
             (2, '2019-1-1', '2019-1-31'),
             (2, '2019-2-1', '2019-2-28'),
             (3, '2018-12-1', '2018-12-31')]
        )
    

if __name__ == '__main__':
    unittest.main()
