import math

from data_structure import *
from scheduling_util import *
import util


class Fragmentizer(object):

    def __init__(self):
        pass

    def _get_max_amount_by_max_percentage(self, max_percentage, schedule):
        schedule_start = schedule.get_schedule_start()
        today_usable_time = schedule.get_real_usable_time(schedule_start)
        return max(0, int(today_usable_time * max_percentage))

    def _get_max_amount_by_min_etr(self, min_etr, schedule, stress_info):
        schedule_start = schedule.get_schedule_start()
        schedule_end = schedule.get_schedule_end()

        result = math.inf

        acc_usable_time = 0
        date = schedule_start

        max_used_ratio = etr_to_used_ratio(min_etr)

        while date <= schedule_end:
            acc_free_time = stress_info.daily_stress_infos[date].acc_free_time.value
            acc_usable_time += schedule.get_usable_time(date)

            current_stress = None
            if acc_usable_time > 0:
                current_stress = 1.0 - (float(acc_free_time) / acc_usable_time)

            else:
                current_stress = 0.0

            if current_stress >= max_used_ratio:
                result = 0
                break

            target_free_time = (1.0 - max_used_ratio) * acc_usable_time

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
        min_etr = fragmentation_config.min_extra_time_ratio.value
        preferred_fragment_size = fragmentation_config.preferred_fragment_size.value
        min_fragment_size = fragmentation_config.min_fragment_size.value

        # compute maximum time we can have

        max_amount_by_max_percentage = self._get_max_amount_by_max_percentage(
            max_percentage, schedule)
        max_amount_by_min_etr = self._get_max_amount_by_min_etr(
            min_etr, schedule, stress_info)
        max_amount = min(max_amount_by_max_percentage, max_amount_by_min_etr)

        if max_amount <= 0:
            return []

        # compute the task to use

        if len(tasks) == 0:
            return []

        result = []

        weights = [1 for task in tasks]
        sampler = Sampler(seed=schedule_start.encode())
        num_fragments, fragment_size = self._divide_evenly(
            max_amount, preferred_fragment_size)
        if fragment_size < min_fragment_size:
            return []

        for i in range(num_fragments):
            fragment = sampler.sample(weights, tasks)

            session = Session()
            session.id.value = fragment.id.value
            session.amount.value = fragment_size
            session.type.value = SessionTypeEnum.FRAGMENT
            session.weakness.value = SessionWeaknessEnum.WEAK

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
