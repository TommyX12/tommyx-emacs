import json, sys, os

# make this directory accessible to subdirectory files
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from server import STDIOServer
from protocol import JSONProtocol
from engine.completer_interface import CompleterInterface
from engine.completer import SmartCompleter

def main():
	completer = SmartCompleter()
	completer_interface = CompleterInterface(completer)
	protocol = JSONProtocol()
	server = STDIOServer(protocol, completer_interface.request_handler)
	server.listen()

if __name__ == '__main__':
	main()
