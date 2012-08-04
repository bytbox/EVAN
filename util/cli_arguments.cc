#include "util.hh"
using namespace util;

#include <string>
#include <vector>
using namespace std;

cli_arguments::cli_arguments(int argc, char *argv[]) {
	for (int i=1; i < argc; i++) {
		string s = argv[i];
		command_line.push_back(s);
		if (s[0] == '-')
			options.push_back(s);
		else
			arguments.push_back(s);
	}
}

cli_arguments::cli_arguments(vector<string> cl) : command_line(cl) {
	process();
}

void cli_arguments::process() {
	for (string s : command_line) {
		if (s[0] == '-')
			options.push_back(s);
		else
			arguments.push_back(s);
	}

}

bool cli_arguments::flag(const string &name) {
	for (string s : options)
		if (s.substr(1) == name) return true;
	return false;
}

string cli_arguments::opt(const string &name) {
	return opt(name, "");
}

string cli_arguments::opt(const string &name, const string &def) {
	for (string s : options)
		if (s.substr(1, name.size()) == name && s[name.size()+1] == '=')
			return s.substr(name.size()+2);
	return def;
}

vector<string> cli_arguments::args() {
	return arguments;
}

