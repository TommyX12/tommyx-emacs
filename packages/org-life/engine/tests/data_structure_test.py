import test_util
test_util.allow_parent_import()

from data_structure import *

import unittest

class DataStructureTest(unittest.TestCase):
    '''
    TODO: Test more stuff
    '''

    def test_daily_schedule(self):
        d = DailySchedule(10)
        
        session = Session()
        session.id.value = 1
        session.amount.value = 2
        session.type.value = SessionTypeEnum.TASK
        session.weakness.value = SessionWeaknessEnum.WEAK
        d.add_session(session)
        
        self.assertEqual(d.get_usable_time(), 10)
        self.assertEqual(d.get_free_time(), 8)
        self.assertEqual(d.is_overlimit(), False)
        
        session = Session()
        session.id.value = 2
        session.amount.value = 4
        session.type.value = SessionTypeEnum.TASK
        session.weakness.value = SessionWeaknessEnum.STRONG
        d.add_session(session)
        
        self.assertEqual(d.get_usable_time(), 6)
        self.assertEqual(d.get_free_time(), 4)
        self.assertEqual(d.is_overlimit(), False)
        
        session = Session()
        session.id.value = 2
        session.amount.value = 10
        session.type.value = SessionTypeEnum.TASK
        session.weakness.value = SessionWeaknessEnum.STRONG
        d.add_session(session)
        
        self.assertEqual(d.get_usable_time(), 0)
        self.assertEqual(d.get_free_time(), 0)
        self.assertEqual(d.is_overlimit(), True)
        
        session = Session()
        session.id.value = 2
        session.amount.value = 10
        session.type.value = SessionTypeEnum.TASK
        session.weakness.value = SessionWeaknessEnum.WEAK
        d.add_session(session)
        
        self.assertEqual(d.get_usable_time(), 0)
        self.assertEqual(d.get_free_time(), 0)
        self.assertEqual(d.is_overlimit(), True)

        s1 = [
            s for s in d.get_sessions()
            if s.id.value == 2 and s.type.value == SessionTypeEnum.TASK and s.weakness.value == SessionWeaknessEnum.STRONG
        ][0]
        self.assertEqual(s1.amount.value, 14)

        s2 = [
            s for s in d.get_sessions()
            if s.id.value == 2 and s.type.value == SessionTypeEnum.TASK and s.weakness.value == SessionWeaknessEnum.WEAK
        ][0]
        self.assertEqual(s2.amount.value, 10)

    def test_progress_info(self):
        task = Task()
        task.amount.value = 100
        task.done.value = 20
        tasks = [task for i in range(5)]
        p1 = ProgressInfo(tasks)
        p1.add_amount_done(1, 5)
        p1.add_amount_done(2, 10)
        p1.add_amount_done(3, 7)
        p2 = ProgressInfo(tasks, None, True)
        p2.add_amount_done(2, 2)
        p2.add_amount_done(3, 8)
        p2.add_amount_done(4, 1)
        p3 = p1.combine(p2)
        self.assertEqual(p1.get_amount_done(0), 20)
        self.assertEqual(p1.get_amount_done(2), 30)
        self.assertEqual(p3.get_amount_done(0), 20)
        self.assertEqual(p3.get_amount_done(1), 25)
        self.assertEqual(p3.get_amount_done(2), 32)
        self.assertEqual(p3.get_amount_done(3), 35)
        self.assertEqual(p3.get_amount_done(4), 21)

    def test_urgency_error(self):
        print_things = False

        def test(thing):
            if print_things:
                print(thing)

        t = Task()

        test('----------')
        t.end = Date.max()
        s = "<2019-03-19 Tue .+1d>: to-next, <2019-06-17 Mon .+3m>: 10"
        u = Urgency().decode_self(s)
        t.urgency = u
        t.parse_urgency()
        test(t.urgency_points)
        test(t.get_urgency(-123))
        test(t.get_urgency(-7))
        test(t.get_urgency(-6))
        test(t.get_urgency(0))
        test(t.get_urgency(3))
        test(t.get_urgency(26))
        test(t.get_urgency(83))

        test('----------')
        t.end = Date.today().add_days(23)
        s = "<2019-03-19 Tue .+1d>: to-deadline, +9: 10"
        u = Urgency().decode_self(s)
        t.urgency = u
        t.parse_urgency()
        test(t.urgency_points)
        test(t.get_urgency(-123))
        test(t.get_urgency(-7))
        test(t.get_urgency(-6))
        test(t.get_urgency(0))
        test(t.get_urgency(2))
        test(t.get_urgency(3))
        test(t.get_urgency(26))
        test(t.get_urgency(83))

        test('----------')
        t.end = Date.max()
        s = "42"
        u = Urgency().decode_self(s)
        t.urgency = u
        t.parse_urgency()
        test(t.urgency_points)
        test(t.get_urgency(-123))
        test(t.get_urgency(-7))
        test(t.get_urgency(-6))
        test(t.get_urgency(0))
        test(t.get_urgency(2))
        test(t.get_urgency(3))
        test(t.get_urgency(26))
        test(t.get_urgency(83))

        test('----------')
        t.end = Date.today().add_days(28)
        s = "42"
        u = Urgency().decode_self(s)
        t.urgency = u
        t.parse_urgency()
        test(t.urgency_points)
        test(t.maximum_urgency)

        test('----------')
        t.end = Date.today().add_days(28)
        s = ""
        u = Urgency().decode_self(s)
        t.urgency = u
        t.parse_urgency()
        test(t.urgency_points)
        test(t.maximum_urgency)
        test(t.get_urgency(-123))
        test(t.get_urgency(-7))
        test(t.get_urgency(-6))
        test(t.get_urgency(0))
        test(t.get_urgency(2))
        test(t.get_urgency(3))
        test(t.get_urgency(11))
        test(t.get_urgency(26))
        test(t.get_urgency(83))

        test('----------')
        t.end = Date.today()
        s = "<2019-03-28 .+1d>: to-next, +60: 10"
        u = Urgency().decode_self(s)
        t.urgency = u
        t.parse_urgency()
        test(t.urgency_points)
        test(t.maximum_urgency)
        test(t.get_urgency(-123))
        test(t.get_urgency(-7))
        test(t.get_urgency(-6))
        test(t.get_urgency(0))
        test(t.get_urgency(2))
        test(t.get_urgency(3))
        test(t.get_urgency(11))
        test(t.get_urgency(26))
        test(t.get_urgency(83))



if __name__ == '__main__':
    unittest.main()
