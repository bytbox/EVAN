#include "builtins.hh"
#include "util.hh"
using namespace util;

#include <fstream>
#include <iostream>
using namespace std;

int main(int argc, char *argv[]) {
	cli_arguments args(argc, argv);
	if (args.flag("h") || args.flag("help")) {
		cout << "usage: " << argv[0] << " [options]" << endl;
		cout << "options:" << endl;
		cout << "\t-serve    serve docs via HTTP" << endl;
		cout << "\t-out=FILE output docs to FILE (default: builtin.html)" << endl;
		return 0;
	}

	if (args.flag("serve")) {
		cerr << "Doc server not yet implemented" << endl;
		return 1;
	}

	string outFilename = args.opt("out", "builtins.html");
	ofstream fout(outFilename);
	for (Category c : BuiltinInfo::categories) {
		fout << "<h2>" << c.name << "</h2>" << endl;
		for (Builtin b : c.builtins) {
			fout << "<h3>" << b.name << "</h3>" << endl;
			fout << b.description << endl;
		}
	}
	fout.close();

	return 0;
}

