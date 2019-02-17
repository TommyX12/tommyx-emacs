import math

from data_structure import *
from scheduling_util import *
import util

class StressAnalyzer(object):

    def __init__(self):
        pass

    def set_logger(self, logger):
        self.logger = logger

    def stress_formula(self, used_ratio, extra_time_ratio):
        '''
        TODO: enhance this
        '''
        return used_ratio

    def get_days_to_deadline(self, session_date, task_end):
        if task_end.is_max():
            return 'inf'

        return session_date.days_to(task_end)

    def compute_session_extra_info(self, date, sessions, tasks):
        for session in sessions:
            if session.task_index is None:
                continue

            task = tasks[session.task_index.value]
            session.to_deadline = Days(self.get_days_to_deadline(date, task.end))

    def get_highest_stress_task(self, tasks, highest_stress_date):
        result = None
        result_amount = 0
        for task in tasks:
            if task.end == highest_stress_date and task.amount.value > result_amount:
                result = task
                result_amount = task.amount.value

        return result

    def analyze_failure_probability(
            self,
            tasks,
            tasks_mask,
            schedule,
            progress_info,
            probability_estimator,
            without_today = False,
        ):
        '''
        TODO
        '''

        p_estimator = probability_estimator
        
        # sort tasks by deadline
        sorted_tasks = [(i, tasks[i]) for i in range(len(tasks))]
        sorted_tasks.sort(key = lambda x : x[1].end)

        # compute link from index back to order
        # self.index_to_order = [0 for _ in range(len(tasks))]
        # for i in range(len(self.sorted_tasks)):
        #     self.index_to_order[self.sorted_tasks[i][0]] = i
    
        # initialization
        sorted_task_pressure = [0 for _ in range(len(sorted_tasks))]
        sorted_task_total_time = [0 for _ in range(len(sorted_tasks))]
        
        # compute early schedule
        start_day = schedule.get_schedule_start().add_days(-1) # -1 is useful
        end_day = schedule.get_schedule_end()
        schedule_days = start_day.days_to(end_day)
        schedule_free_time_until = [0 for _ in range(schedule_days)]
        for i in range(1, schedule_days):
            if i == 1 and without_today:
                schedule_free_time_until[i] = 0
                
            else:
                schedule_free_time_until[i] = schedule.get_free_time(start_day.add_days(i)) + schedule_free_time_until[i - 1]

        # compute total & variable time
        prev_variable_time = 0
        for i in range(len(sorted_tasks)):
            if not tasks_mask[sorted_tasks[i][0]]:
                continue

            variable_time = progress_info.get_amount_left(sorted_tasks[i][0]) + prev_variable_time
            sorted_task_pressure[i] = variable_time
            prev_variable_time = variable_time
            
        for i in range(len(sorted_tasks)):
            due = max(1, start_day.days_to(sorted_tasks[i][1].end))
            if due < schedule_days:
                sorted_task_total_time[i] = schedule_free_time_until[due]

            else:
                sorted_task_total_time[i] = math.inf

        # def debug(i):
        #     self.logger.log("prob {}, pres {}, total {}".format(
        #         p_estimator.get_success_probability(
        #             sorted_tasks[i][1],
        #             sorted_task_pressure[i],
        #             sorted_task_total_time[i]
        #         ),
        #         sorted_task_pressure[i],
        #         sorted_task_total_time[i]
        #     ))
        #     return None

        pof = max([
            1 - p_estimator.get_success_probability(
                sorted_tasks[i][1],
                sorted_task_pressure[i],
                sorted_task_total_time[i]
            )
            for i in range(len(sorted_tasks))
            if tasks_mask[sorted_tasks[i][0]] # and debug(i) is None
        ])
        # self.logger.log(str([
        #     (1 - p_estimator.get_success_probability(
        #         sorted_tasks[i][1],
        #         sorted_task_pressure[i],
        #         sorted_task_total_time[i]
        #     ), sorted_tasks[i][1].end.encode(),
        #      sorted_task_pressure[i],
        #      sorted_task_total_time[i]
        #     )
        #     for i in range(len(sorted_tasks))
        #     if tasks_mask[sorted_tasks[i][0]] # and debug(i) is None
        # ]))

        highest_workload = -math.inf
        schedule_start = schedule.get_schedule_start()
        highest_workload_date = schedule_start
        for i in range(len(sorted_tasks)):
            if not tasks_mask[sorted_tasks[i][0]]:
                continue

            workload = sorted_task_pressure[i] / sorted_task_total_time[i] if sorted_task_total_time[i] != 0 else math.inf
            if workload > highest_workload and not sorted_tasks[i][1].end.is_max():
                highest_workload = workload
                highest_workload_date = max(schedule_start, sorted_tasks[i][1].end)
        
        return pof, highest_workload, highest_workload_date

    def analyze_late_schedule(self, schedule, bias = 0):
        '''
        Precondition: Sessions in schedule are stress-contributing.
            This means it is the least optimal schedule and has to be done.
        
        TODO: This is wrong
        TODO: Need to test bias
        TODO: Need to test highest_stress_date
        '''
        schedule_start = schedule.get_schedule_start()
        schedule_end = schedule.get_schedule_end()

        overall_used_ratio = 0.0
        stress_info = StressInfo()

        bias = min(bias, schedule.get_usable_time(schedule_start))
        acc_free_time = -bias
        acc_usable_time = -bias

        highest_stress_date = schedule_start
        
        date = schedule_start
        while date <= schedule_end:
            acc_free_time += schedule.get_free_time(date)
            acc_usable_time += schedule.get_usable_time(date)

            if schedule.is_overlimit(date) or acc_free_time < 0:
                acc_used_ratio = 1.0
                acc_free_time = 0

            else:
                if acc_usable_time <= 0:
                    acc_used_ratio = 0.0

                else:
                    acc_used_ratio = 1.0 - (acc_free_time / acc_usable_time)

            if acc_used_ratio >= overall_used_ratio:
                overall_used_ratio = acc_used_ratio
                highest_stress_date = date

            daily_stress_info = DailyStressInfo()
            daily_stress_info.acc_free_time.value = acc_free_time
            acc_extra_time_ratio = used_ratio_to_etr(acc_used_ratio)
            daily_stress_info.acc_extra_time_ratio.value = acc_extra_time_ratio
            daily_stress_info.acc_average_stress.value = self.stress_formula(acc_used_ratio, acc_extra_time_ratio)

            stress_info.daily_stress_infos[date] = daily_stress_info
            
            date = date.add_days(1)

        extra_time_ratio = used_ratio_to_etr(overall_used_ratio)
        stress_info.extra_time_ratio.value = extra_time_ratio
        stress_info.overall_stress.value = self.stress_formula(overall_used_ratio, extra_time_ratio)
        stress_info.highest_stress_date = highest_stress_date

        return stress_info


def main():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    main()
