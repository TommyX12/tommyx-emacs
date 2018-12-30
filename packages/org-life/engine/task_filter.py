
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
            task.end <= schedule_end
            for task in tasks
        ]

    def get_bad_estimate_tasks(self, tasks, progress_info):
        result = []
        for i in range(len(tasks)):
            task = tasks[i]
            done = task.done.value + progress_info.get_done_amount(i)
            if done > task.amount.value:
                bad_estimate_task = BadEstimateTask()
                bad_estimate_task.id.value = task.id.value
                bad_estimate_task.amount.value = task.amount.value
                bad_estimate_task.done.value = done

                result.append(bad_estimate_task)

        return result

