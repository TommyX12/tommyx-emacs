import math

from data_structure import *
from scheduling_util import *
import util


class Fragmentizer(object):

    def __init__(self):
        pass

    def _get_max_amount_by_max_percentage(self, max_percentage, schedule, stress_info):
        schedule_start = schedule.get_schedule_start()
        
        today_free_time = stress_info.daily_stress_infos[schedule_start].acc_free_time.value
        return max(0, int(today_free_time * max_percentage))

    def _get_max_amount_by_max_stress(self, max_stress, schedule, stress_info):
        schedule_start = schedule.get_schedule_start()
        schedule_end = schedule.get_schedule_end()

        result = math.inf

        acc_work_time = 0
        date = schedule_start
        while date <= schedule_end:
            acc_free_time = stress_info.daily_stress_infos[date].acc_free_time.value
            acc_work_time += schedule.get_work_time(date)
            
            current_stress = 1.0 - (float(acc_free_time) / acc_work_time)
            if current_stress >= max_stress:
                result = 0
                break

            target_free_time = (1.0 - max_stress) * acc_work_time

            result = min(result, acc_free_time - target_free_time)

            date = date.add_days(1)

        return max(0, int(result))

    def _divide_evenly(self, total, target_chunk_size):
        num_chunks = max(1, int(round(total / target_chunk_size)))
        chunk_size = total // num_chunks
        return num_chunks, chunk_size

    def suggest_fragments(self, tasks, schedule, stress_info, fragmentation_config):
        schedule_start = schedule.get_schedule_start()
        schedule_end = schedule.get_schedule_end()

        max_percentage = fragmentation_config.max_percentage.value
        max_stress = fragmentation_config.max_stress.value
        preferred_fragment_size = fragmentation_config.preferred_fragment_size.value
        min_fragment_size = fragmentation_config.min_fragment_size.value

        # compute maximum time we can have
        
        max_amount_by_max_percentage = self._get_max_amount_by_max_percentage(max_percentage, schedule, stress_info)
        max_amount_by_max_stress = self._get_max_amount_by_max_stress(max_stress, schedule, stress_info)
        max_amount = min(max_amount_by_max_percentage, max_amount_by_max_stress)

        if max_amount <= 0:
            return []

        # compute the task to use

        if len(tasks) == 0:
            return []

        result = []

        weights = [1 for task in tasks]
        sampler = Sampler(weights, seed = schedule_start.encode())
        num_fragments, fragment_size = self._divide_evenly(max_amount, preferred_fragment_size)
        if fragment_size < min_fragment_size:
            return []

        for i in range(num_fragments):
            fragment = sampler.sample(tasks)
            
            session = Session()
            session.id.value = fragment.id.value
            session.amount.value = fragment_size
            session.type.value = SessionTypeEnum.FRAGMENT

            dated_session = DatedSession()
            dated_session.date = schedule_start
            dated_session.session = session

            result.append(dated_session)

        return result


def main():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    main()
