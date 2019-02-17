import test_util
test_util.allow_parent_import()

from stress_analyzer import *
from data_structure import *
from usable_time_parser import *

import unittest

class StressAnalyzerTest(unittest.TestCase):
    
    def test_stress_analyzer(self):
        schedule = test_util.make_schedule([10, 10, 10, 20, 30], [10, 5, 0, 0, 30])
        s = StressAnalyzer()
        r = s.analyze_late_schedule(schedule)
        self.assertEqual(
            [
                r.daily_stress_infos[schedule.get_schedule_start().add_days(i)].acc_free_time.value
                for i in range(5)
            ],
            [10, 15, 15, 15, 45]
        )
        self.assertEqual(
            [
                round(r.daily_stress_infos[schedule.get_schedule_start().add_days(i)].acc_average_stress.value, 2)
                for i in range(5)
            ],
            [0.0, 0.25, 0.5, 0.7, 0.44]
        )
        self.assertEqual(r.overall_stress.value, 0.7)


if __name__ == '__main__':
    unittest.main()
