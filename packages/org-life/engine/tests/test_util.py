import os, sys

def allow_parent_import():
    sys.path.append(os.path.join(os.path.dirname(os.path.realpath(__file__)), os.pardir))
