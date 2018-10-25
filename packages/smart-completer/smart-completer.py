import json

if __name__ == '__main__':
	while True:
		message = json.loads(str(input()))
		prefix = message['prefix']
		results = {
			'candidates': [
				(prefix + "what"),
				(prefix + "wht"),
				(prefix + "interactive"),
				(prefix + "inwg"),
				(prefix + "interavve"),
				(prefix + "interaction")
			],
		}
		print(results)
