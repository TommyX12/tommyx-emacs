import test_util
test_util.allow_parent_import()

from fragmentizer import *
from data_structure import *
from work_time_parser import *

import unittest

class FragmentizerTest(unittest.TestCase):
    '''
    TODO: Test min_fragment_size
    '''
    
    def test_get_max_amount_by_max_percentage(self):
        max_percentage = 0.2
        schedule = test_util.make_schedule([20, 5, 4, 2, 30])
        stress_info = test_util.make_stress_info([10, 5, 4, 2, 30])
        f = Fragmentizer()
        self.assertEqual(f._get_max_amount_by_max_percentage(max_percentage, schedule, stress_info), 2)
    
    def test_get_max_amount_by_max_stress(self):
        max_stress = 0.8
        schedule = test_util.make_schedule([10, 10, 10, 20, 30])
        stress_info = test_util.make_stress_info([10, 5, 0, 0, 30])
        f = Fragmentizer()
        self.assertEqual(f._get_max_amount_by_max_stress(max_stress, schedule, stress_info), 5)
    
    def test_divide_evenly(self):
        f = Fragmentizer()
        self.assertEqual(f._divide_evenly(100, 30), (3, 33))
        self.assertEqual(f._divide_evenly(100, 40), (2, 50))
        self.assertEqual(f._divide_evenly(100, 35), (3, 33))
        self.assertEqual(f._divide_evenly(50, 25), (2, 25))
    
    def test_suggest_fragments(self):
        task1 = Task()
        task1.id.value = 1
        task2 = Task()
        task2.id.value = 2
        task3 = Task()
        task3.id.value = 3
        task4 = Task()
        task4.id.value = 4
        tasks = [task1, task2, task3, task4]
        max_percentage = 0.6
        max_stress = 0.8
        fragment_size = 2
        schedule = test_util.make_schedule([10, 10, 10, 20, 30])
        stress_info = test_util.make_stress_info([10, 5, 0, 0, 30])
        fragmentation_config = FragmentationConfig()
        fragmentation_config.max_percentage.value = max_percentage
        fragmentation_config.max_stress.value = max_stress
        fragmentation_config.preferred_fragment_size.value = fragment_size
        f = Fragmentizer()
        r = f.suggest_fragments(
            tasks, schedule, stress_info, fragmentation_config
        )
        self.assertEqual(len(r), 2)
        for s in r:
            self.assertEqual(s.date, schedule.get_schedule_start())
            
        self.assertEqual([
            s.session.amount.value
            for s in r
        ], [2, 2])

        i = r[0].session.id.value
        for _ in range(10):
            r = f.suggest_fragments(
                tasks, schedule, stress_info, fragmentation_config
            )
            j = r[0].session.id.value
            self.assertEqual(i, j)


if __name__ == '__main__':
    unittest.main()
