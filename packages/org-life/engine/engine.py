import util

from logger import DummyLogger

class FillDirection(enum):
    EARLY = 0
    LATE = 1
    

class Schedule(object):
    
    def __init__(self, config, work_time_config):
        raise NotImplementedError()
    
    def copy(self):
        raise NotImplementedError()
    
    def plan(self, tasks, direction = FillDirection.EARLY):
        raise NotImplementedError()
    
    def count_free_time(self):
        raise NotImplementedError()
    

class Engine(object):

    def __init__(self, logger = DummyLogger()):
        self.logger = logger

    def schedule(self, scheduling_request):
        # setup
        config = scheduling_request.config
        tasks = scheduling_request.tasks
        work_time_config = scheduling_request.work_time

        # parse work time configuration
        # generate array of work time.
        early_schedule = Schedule(config, work_time_config)
        late_schedule = early_schedule.copy()

        # progress count
        # right now we just use given "done" progress.
        # we add more future progress by doing the counts

        # free time and conflict check
        # run backward pass to generate maximum free time, percentage, and overall stress
        # report impossible tasks to alert

        # schedule suggestion for fragmented tasks (today or more)
        # compute fragmented time for today (using maximum free time)
        # have the randomizer seeded with today's day string.
        # select task randomly from all tasks

        # schedule suggestion for deadline tasks, as well as task stress
        # run forward pass (while accounting for today's fragmented time) to generate suggestion
        # compute task stress of each session

