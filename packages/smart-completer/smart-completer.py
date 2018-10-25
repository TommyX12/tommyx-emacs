import json
from server import STDIOServer
from protocol import JSONProtocol
from engine.completer import Completer

def main():
	completer = Completer()
	server = STDIOServer(JSONProtocol(), completer.request_handler)
	server.listen()

if __name__ == '__main__':
	main()
