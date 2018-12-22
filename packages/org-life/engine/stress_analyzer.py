
from data_structure import *


class StressAnalyzer(object):

    def __init__(self):
        pass

    def analyze(self, schedule, bias = 0):
        '''
        Precondition: Sessions in schedule are stress-contributing.
            This means it is the least optimal schedule and has to be done.
        
        TODO: Need to test bias
        TODO: Need to test highest_stress_date
        '''
        schedule_start = schedule.get_schedule_start()
        schedule_end = schedule.get_schedule_end()

        overall_stress = 0.0
        stress_info = StressInfo()

        bias = min(bias, schedule.get_usable_time(schedule_start))
        acc_free_time = -bias
        acc_usable_time = -bias

        highest_stress_date = schedule_start
        
        date = schedule_start
        while date <= schedule_end:
            acc_free_time += schedule.get_free_time(date)
            acc_usable_time += schedule.get_usable_time(date)

            if acc_free_time < 0:
                acc_average_stress = 1.0
                acc_free_time = 0

            else:
                if acc_usable_time <= 0:
                    acc_average_stress = 0.0

                else:
                    acc_average_stress = 1.0 - (acc_free_time / acc_usable_time)

            if acc_average_stress >= overall_stress:
                overall_stress = acc_average_stress
                highest_stress_date = date

            daily_stress_info = DailyStressInfo()
            daily_stress_info.acc_free_time.value = acc_free_time
            daily_stress_info.acc_average_stress.value = acc_average_stress

            stress_info.daily_stress_infos[date] = daily_stress_info
            
            date = date.add_days(1)

        stress_info.overall_stress.value = overall_stress
        stress_info.highest_stress_date = highest_stress_date

        return stress_info


def main():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    main()
