#include "util.hh"
using namespace util;

#include <string>
#include <vector>
using namespace std;

cli_arguments::cli_arguments(int argc, char *argv[]) {
	for (int i=1; i < argc; i++)
		command_line.push_back(argv[i]);
}

cli_arguments::cli_arguments(vector<string> cl) : command_line(cl) {}

bool cli_arguments::flag(const string &name) {
	for (string s : command_line) {
		if (s[0] == '-' && s.substr(1) == name) return true;
	}
	return false;
}

string cli_arguments::opt(const string &name) {
	return "";
}

