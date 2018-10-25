import sys
import traceback

def print_err(message):
	print(message, file=sys.stderr)

def print_log(message):
	print(message, file=sys.stderr)

def print_traceback():
	traceback.print_exc()
