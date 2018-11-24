import json, sys, os

# make this directory accessible to subdirectory files
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from server import STDIOServer
from engine import Engine
from engine_controller import EngineController
from serializer import JSONSerializer

def main():
    engine = Engine()
    serializer = JSONSerializer()
    engine_controller = EngineController(engine, serializer)
    server = STDIOServer(engine_controller.request_handler)
    server.listen()

if __name__ == '__main__':
    main()
