#include <iostream>
#include <cstdio>
#include <string>
#include <vector>

using namespace std;

string joinVector(const vector<string>& strings, char sep) {
	string result;
	for (int i = 0; i < strings.size(); ++i) {
		if (i) {
			result += sep;
		}
		result += strings[i];
	}
	return result;
}

int main(int argc, char *argv[]) {

	string prefix;
	while (1) {
		cin >> prefix;
		vector<string> results;
		results.push_back(prefix + "what");
		results.push_back(prefix + "wht");
		results.push_back(prefix + "interactive");
		results.push_back(prefix + "inwg");
		results.push_back(prefix + "interavve");
		results.push_back(prefix + "interaction");
		cout << joinVector(results, '\t') << endl;
	}
    
	return 0;
}
