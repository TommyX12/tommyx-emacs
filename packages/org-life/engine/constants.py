import re

SINGLE_DATE_RE = re.compile(r'^\d+-\d+-\d+$')
DATE_RANGE_RE = re.compile(r'^\d+-\d+-\d+\s+-\s+\d+-\d+-\d+$')
DATE_RANGE_SPLIT_RE = re.compile(r'\s+-\s+')

SINGLE_DATE_INLINE_RE = re.compile(r'\d+-\d+-\d+')

INF_URGENCY = 1000000

FRAGMENT_ENABLED = False
