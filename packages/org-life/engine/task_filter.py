
from data_structure import *
from scheduling_util import *
import util

class TaskFilter(object):

    def __init__(self):
        pass

    def get_todo_tasks(self, tasks):
        return [
            task for task in tasks
            if task.status.value == TaskStatusEnum.TODO
        ]

    def get_stress_contributor_tasks_mask(self, tasks, schedule_start, schedule_end):
        '''
        Note that we assume schedule_start is today, which means we still care even if task.start < schedule_start.
        '''
        return [
            (not task.stressless.value) and task.end <= schedule_end
            for task in tasks
        ]

    def assign_implicit_stressless(self, tasks, stress_contributor_tasks_mask):
        '''
        TODO: Add tests.
        '''
        for i in range(len(tasks)):
            tasks[i].stressless.value = not stress_contributor_tasks_mask[i]

    def get_bad_estimate_tasks(self, tasks, progress_info):
        result = []
        for i in range(len(tasks)):
            task = tasks[i]
            done = task.done.value + progress_info.get_done_amount(i)
            if task.status.value == TaskStatusEnum.TODO and done >= task.amount.value:
                bad_estimate_task = BadEstimateTask()
                bad_estimate_task.id.value = task.id.value
                bad_estimate_task.amount.value = task.amount.value
                bad_estimate_task.done.value = done

                result.append(bad_estimate_task)

        return result

    def get_bad_info_tasks(self, tasks):
        result = []
        for i in range(len(tasks)):
            task = tasks[i]
            if task.status.value == TaskStatusEnum.TODO and task.amount is None:
                bad_info_task = BadInfoTask()
                bad_info_task.id.value = task.id.value
                bad_info_task.reason.value = 'No Effort'

                result.append(bad_info_task)

        return result

    def get_schedulable_tasks(self, tasks):
        return [
            task for task in tasks
            if task.amount is not None and task.amount.value > 0
        ]

    def get_overdue_tasks(self, tasks, schedule_start):
        result = []
        for i in range(len(tasks)):
            task = tasks[i]
            if task.status.value == TaskStatusEnum.TODO and task.end < schedule_start:
                overdue_task = OverdueTask()
                overdue_task.id.value = task.id.value
                overdue_task.days.value = schedule_start.days_to(task.end)

                result.append(overdue_task)

        return result

