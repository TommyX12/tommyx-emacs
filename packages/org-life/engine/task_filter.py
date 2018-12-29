
from data_structure import *
from scheduling_util import *
import util

class TaskFilter(object):

    def __init__(self):
        pass

    def get_stress_contributor_tasks_mask(self, tasks, schedule_start, schedule_end):
        '''
        Note that we assume schedule_start is today, which means we still care even if task.start < schedule_start.
        '''
        return [
            task.end <= schedule_end
            for task in tasks
        ]
