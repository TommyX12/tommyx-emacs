import test_util
test_util.allow_parent_import()

from data_structure import *
from work_time_parser import *

import unittest

class WorkTimeParserTest(unittest.TestCase):

    def test_work_time_parser(self):
        parser = WorkTimeParser()
        work_time_config = [
            WorkTimeConfigEntry().decode_self(
                {"selector":"default","duration":5}
            ),
            WorkTimeConfigEntry().decode_self(
                {"selector":"saturday","duration":10}
            ),
            WorkTimeConfigEntry().decode_self(
                {"selector":"friday","duration":20}
            ),
            WorkTimeConfigEntry().decode_self(
                {"selector":"2018-12-06","duration":15}
            ),
            WorkTimeConfigEntry().decode_self(
                {"selector":"2018-12-12 - 2018-12-16","duration":30}
            ),
            WorkTimeConfigEntry().decode_self(
                {"selector":"2018-12-14","duration":0}
            ),
        ]
        schedule_start = Date().decode_self('2018-12-06')
        schedule_end = Date().decode_self('2019-01-01')
        d = parser.get_work_time_dict(schedule_start, schedule_end, work_time_config)
        self.assertEqual(d[Date().decode_self('2018-12-06')].value, 15)
        self.assertEqual(d[Date().decode_self('2018-12-07')].value, 20)
        self.assertEqual(d[Date().decode_self('2018-12-08')].value, 10)
        self.assertEqual(d[Date().decode_self('2018-12-09')].value, 5)
        self.assertEqual(d[Date().decode_self('2018-12-11')].value, 5)
        self.assertEqual(d[Date().decode_self('2018-12-12')].value, 30)
        self.assertEqual(d[Date().decode_self('2018-12-13')].value, 30)
        self.assertEqual(d[Date().decode_self('2018-12-14')].value, 0)
        self.assertEqual(d[Date().decode_self('2018-12-15')].value, 30)
        self.assertEqual(d[Date().decode_self('2018-12-20')].value, 5)


if __name__ == '__main__':
    unittest.main()
