import test_util
test_util.allow_parent_import()

from progress_counter import *
from data_structure import *
from usable_time_parser import *

import unittest

class ProgressCounterTest(unittest.TestCase):
    '''
    TODO: Test more
    '''

    def test_progress_counter(self):
        task1 = Task()
        task1.id.value = 1
        task1.start = Date().decode_self('2018-11-01')
        task1.end = Date().decode_self('2018-12-02')
        task2 = Task()
        task2.id.value = 2
        task2.start = Date().decode_self('2018-12-02')
        task2.end = Date().decode_self('2018-12-04')
        task3 = Task()
        task3.id.value = 3
        task3.start = Date().decode_self('2018-12-01')
        task3.end = Date().decode_self('max')
        tasks = [task1, task2, task3]

        ds1 = DatedSession()
        ds1.date = Date().decode_self('2018-12-01')
        ds1.session.id.value = 1
        ds1.session.amount.value = 2
        ds2 = DatedSession()
        ds2.date = Date().decode_self('2018-12-01')
        ds2.session.id.value = 2
        ds2.session.amount.value = 3
        ds3 = DatedSession()
        ds3.date = Date().decode_self('2018-12-01')
        ds3.session.id.value = 3
        ds3.session.amount.value = 5
        ds4 = DatedSession()
        ds4.date = Date().decode_self('2018-12-02')
        ds4.session.id.value = 1
        ds4.session.amount.value = 7
        ds5 = DatedSession()
        ds5.date = Date().decode_self('2018-12-02')
        ds5.session.id.value = 2
        ds5.session.amount.value = 11
        ds6 = DatedSession()
        ds6.date = Date().decode_self('2018-12-02')
        ds6.session.id.value = 3
        ds6.session.amount.value = 13
        ds7 = DatedSession()
        ds7.date = Date().decode_self('2018-12-03')
        ds7.session.id.value = 1
        ds7.session.amount.value = 17
        ds8 = DatedSession()
        ds8.date = Date().decode_self('2018-12-03')
        ds8.session.id.value = 2
        ds8.session.amount.value = 19
        ds9 = DatedSession()
        ds9.date = Date().decode_self('2018-12-03')
        ds9.session.id.value = 3
        ds9.session.amount.value = 23
        ds10 = DatedSession()
        ds10.date = Date().decode_self('2018-12-03')
        ds10.session.id.value = 4
        ds10.session.amount.value = 23
        dated_sessions = [ds1, ds2, ds3, ds4, ds5, ds6, ds7, ds8, ds9, ds10]
        
        p = ProgressCounter()
        r = p.count(tasks, dated_sessions)
        self.assertEqual(r.get_done_amount(0), 9)
        self.assertEqual(r.get_done_amount(1), 30)
        self.assertEqual(r.get_done_amount(2), 41)
    

if __name__ == '__main__':
    unittest.main()
