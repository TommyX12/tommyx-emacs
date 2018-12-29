
from data_structure import *
from scheduling_util import *


class TaskRepeater(object):

    def __init__(self):
        pass

    def repeat_task(self, task, repeat, schedule_start, schedule_end):
        '''
        Generate list of instances of task using repeat.
        The first instance is included to perform clamping.
        '''
        if repeat is None or repeat.unit.value == TaskRepeatUnitEnum.NONE:
            return [task]

        task_duration = task.start.days_to(task.end)
        if task_duration < 0:
            raise ValueError()

        result = []

        if repeat.type.value == TaskRepeatTypeEnum.NORMAL:
            if task.end >= schedule_start:
                result.append(task)

            old_task_end = task.end
            first_task_end = task.end

        elif repeat.type.value == TaskRepeatTypeEnum.RESTART:
            new_task = task.copy()
            if new_task.end < schedule_start:
                new_task.end = schedule_start
            
            result.append(new_task)
            old_task_end = new_task.end
            first_task_end = new_task.end

        else:
            raise ValueError()

        i = 0
        
        while True:
            # generate next copy
            i += 1
            unit = repeat.unit.value
            value = max(1, repeat.value.value)
            if unit == TaskRepeatUnitEnum.DAY:
                new_task_end = first_task_end.add_days(i * value)
            elif unit == TaskRepeatUnitEnum.WEEK:
                new_task_end = first_task_end.add_days(i * 7 * value)
            elif unit == TaskRepeatUnitEnum.MONTH:
                new_task_end = first_task_end.add_months(i * value)
            elif unit == TaskRepeatUnitEnum.YEAR:
                new_task_end = first_task_end.add_years(i * value)
            else:
                raise ValueError()

            new_task_start = new_task_end.add_days(-task_duration)
            if new_task_start <= old_task_end:
                new_task_start = old_task_end.add_days(1)

            # check if in range. break otherwise
            if new_task_start > schedule_end:
                break

            if new_task_end >= schedule_start:
                new_task = task.copy()
                new_task.start = new_task_start
                new_task.end = new_task_end
                new_task.done.value = 0
                new_task.repeat = None
                result.append(new_task)

            old_task_end = new_task_end

        return result

    def repeat(self, tasks, schedule_start, schedule_end):
        '''
        Modify tasks to have all repeat instances generated
        '''

        result = []

        for i in range(len(tasks)):
            result += self.repeat_task(
                tasks[i],
                tasks[i].repeat,
                schedule_start, schedule_end
            )

        return result


def main():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    main()
