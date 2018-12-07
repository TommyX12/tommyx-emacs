import re

from data_structure import *


class WorkTimeParser(object):

    DEFAULT_SELECTOR = 'default'
    DEFAULT_WORK_TIME = Duration(8)
    SELECTOR_TO_WEEKDAY = {
        'monday': 0,
        'tuesday': 1,
        'wednesday': 2,
        'thursday': 3,
        'friday': 4,
        'saturday': 5,
        'sunday': 6,
    }

    SINGLE_DATE_RE = re.compile(r'^\d+-\d+-\d+$')
    DATE_RANGE_RE = re.compile(r'^\d+-\d+-\d+\s+-\s+\d+-\d+-\d+$')
    DATE_RANGE_SPLIT_RE = re.compile(r'\s+-\s+')

    def __init__(self):
        pass

    def _match_single_date(value):
        return WorkTimeParser.SINGLE_DATE_RE.match(value) is not None

    def _parse_single_date(value):
        components = [int(component) for component in value.split('-')]
        return Date.from_components(*components)

    def _match_date_range(value):
        return WorkTimeParser.DATE_RANGE_RE.match(value) is not None

    def _parse_date_range(value):
        dates = WorkTimeParser.DATE_RANGE_SPLIT_RE.split(value)
        return WorkTimeParser._parse_single_date(dates[0]), WorkTimeParser._parse_single_date(dates[1])

    def get_work_time_dict(self, schedule_start, schedule_end, work_time_config):
        '''
        TODO
        
        >>> parser = WorkTimeParser()
        >>> work_time_config = [
        ...     WorkTimeConfigEntry().decode_self(
        ...         {"selector":"default","duration":5}
        ...     ),
        ...     WorkTimeConfigEntry().decode_self(
        ...         {"selector":"saturday","duration":10}
        ...     ),
        ...     WorkTimeConfigEntry().decode_self(
        ...         {"selector":"friday","duration":20}
        ...     ),
        ...     WorkTimeConfigEntry().decode_self(
        ...         {"selector":"2018-12-06","duration":15}
        ...     ),
        ...     WorkTimeConfigEntry().decode_self(
        ...         {"selector":"2018-12-12 - 2018-12-16","duration":30}
        ...     ),
        ...     WorkTimeConfigEntry().decode_self(
        ...         {"selector":"2018-12-14","duration":0}
        ...     ),
        ... ]
        >>> schedule_start = Date().decode_self('2018-12-06')
        >>> schedule_end = Date().decode_self('2019-01-01')
        >>> d = parser.get_work_time_dict(schedule_start, schedule_end, work_time_config)
        >>> d[Date().decode_self('2018-12-06')].value
        15
        >>> d[Date().decode_self('2018-12-07')].value
        20
        >>> d[Date().decode_self('2018-12-08')].value
        10
        >>> d[Date().decode_self('2018-12-09')].value
        5
        >>> d[Date().decode_self('2018-12-11')].value
        5
        >>> d[Date().decode_self('2018-12-12')].value
        30
        >>> d[Date().decode_self('2018-12-13')].value
        30
        >>> d[Date().decode_self('2018-12-14')].value
        0
        >>> d[Date().decode_self('2018-12-15')].value
        30
        >>> d[Date().decode_self('2018-12-20')].value
        5
        '''
        default = Duration(WorkTimeParser.DEFAULT_WORK_TIME.value)
        result = {}
        day_of_week_dict = {}

        # parse day_of_week_dict
        for entry in work_time_config:
            if entry.selector.value == WorkTimeParser.DEFAULT_SELECTOR:
                default = Duration(entry.duration.value)

            elif entry.selector.value in WorkTimeParser.SELECTOR_TO_WEEKDAY:
                day_of_week_dict[
                    WorkTimeParser.SELECTOR_TO_WEEKDAY[entry.selector.value]
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
        for entry in work_time_config:
            if WorkTimeParser._match_single_date(entry.selector.value):
                date = WorkTimeParser._parse_single_date(entry.selector.value)
                result[date] = Duration(entry.duration.value)

            elif WorkTimeParser._match_date_range(entry.selector.value):
                date_start, date_end = WorkTimeParser._parse_date_range(entry.selector.value)
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
