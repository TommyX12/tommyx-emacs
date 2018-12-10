import os, sys

def allow_parent_import():
    sys.path.append(os.path.join(os.path.dirname(os.path.realpath(__file__)), os.pardir))

allow_parent_import()

from data_structure import *

def make_schedule(work_times, free_times = None):
    if free_times is None:
        free_times = work_times[:]

    schedule_start = Date().decode_self('2018-12-01')
    schedule_end = schedule_start.add_days(len(work_times) - 1)
    work_time_dict = {
        schedule_start.add_days(i): Duration(work_times[i])
        for i in range(len(work_times))
    }
    schedule = Schedule.from_work_time_dict(schedule_start, schedule_end, work_time_dict)
    date = schedule_start
    i = 0
    while date <= schedule_end:
        session = Session()
        session.amount.value = work_times[i] - free_times[i]
        schedule.add_session(date, session)
        date = date.add_days(1)
        i += 1
        
    return schedule

def make_stress_info(free_times, average_stress_list = None):
    schedule_start = Date().decode_self('2018-12-01')
    schedule_end = schedule_start.add_days(len(free_times) - 1)
    for i in range(1, len(free_times)):
        free_times[i] += free_times[i - 1]
        
    if average_stress_list == None:
        average_stress_list = [Ratio(0) for free_time in free_times]

    daily_stress_infos = {
        schedule_start.add_days(i):
            DailyStressInfo().set_property('acc_free_time', Duration(free_times[i]))
                             .set_property('acc_average_stress', Ratio(average_stress_list[i]))
        
        for i in range(len(free_times))
    }
    stress_info = StressInfo()
    stress_info.daily_stress_infos = daily_stress_infos
    return stress_info


