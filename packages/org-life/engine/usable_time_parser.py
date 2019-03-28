import re

from data_structure import *
from constants import *


class UsableTimeParser(object):

    DEFAULT_SELECTOR = 'default'
    DEFAULT_USABLE_TIME = Duration(8)
    SELECTOR_TO_WEEKDAY = {
        'monday': 0,
        'tuesday': 1,
        'wednesday': 2,
        'thursday': 3,
        'friday': 4,
        'saturday': 5,
        'sunday': 6,
    }

    def __init__(self):
        pass

    def _match_single_date(value):
        return SINGLE_DATE_RE.match(value) is not None

    def _parse_single_date(value):
        components = [int(component) for component in value.split('-')]
        return Date.from_components(*components)

    def _match_date_range(value):
        return DATE_RANGE_RE.match(value) is not None

    def _parse_date_range(value):
        dates = DATE_RANGE_SPLIT_RE.split(value)
        return UsableTimeParser._parse_single_date(dates[0]), UsableTimeParser._parse_single_date(dates[1])

    def get_usable_time_dict(self, schedule_start, schedule_end, usable_time_config):
        default = Duration(UsableTimeParser.DEFAULT_USABLE_TIME.value)
        result = {}
        day_of_week_dict = {}

        # parse day_of_week_dict
        for entry in usable_time_config:
            if entry.selector.value == UsableTimeParser.DEFAULT_SELECTOR:
                default = Duration(entry.duration.value)

            elif entry.selector.value in UsableTimeParser.SELECTOR_TO_WEEKDAY:
                day_of_week_dict[
                    # UsableTimeSelector made sure that entry.selector.value is lower case
                    UsableTimeParser.SELECTOR_TO_WEEKDAY[entry.selector.value]
                ] = Duration(entry.duration.value)

        # default and weekday selectors
        date = schedule_start
        while date <= schedule_end:
            weekday = date.get_weekday()
            if weekday in day_of_week_dict:
                result[date] = Duration(day_of_week_dict[weekday].value)

            else:
                result[date] = Duration(default.value)

            date = date.add_days(1)

        # date selectors
        for entry in usable_time_config:
            if UsableTimeParser._match_single_date(entry.selector.value):
                date = UsableTimeParser._parse_single_date(entry.selector.value)
                result[date] = Duration(entry.duration.value)

            elif UsableTimeParser._match_date_range(entry.selector.value):
                date_start, date_end = UsableTimeParser._parse_date_range(entry.selector.value)
                if date_start < schedule_start:
                    date_start = schedule_start

                if schedule_end < date_end:
                    date_end = schedule_end

                date = date_start
                while date <= date_end:
                    result[date] = Duration(entry.duration.value)
                    date = date.add_days(1)


        return result


def main():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    main()
