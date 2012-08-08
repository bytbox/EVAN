#include "util.hh"
using namespace util;

#include <iostream>
using namespace std;

int main(int argc, char *argv[]) {
	cli_arguments args(argc, argv);
	if (args.flag("h") || args.flag("help")) {
		cout << "usage: " << argv[0] << " [options]" << endl;
		cout << "options:" << endl;
		return 0;
	}

	return 0;
}

