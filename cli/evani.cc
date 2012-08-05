#include "parse.hh"

#include "interp.hh"
#include "program.hh"
#include "util.hh"
using namespace util;

#include <iostream>
#include <string>
using namespace std;

#include <cstdio>

int main(int argc, char *argv[]) {
	cli_arguments args(argc, argv);
	if (args.flag("h") || args.flag("help")) {
		cout << "usage: " << argv[0] << " [options]" << endl;
		cout << "options:" << endl;
		return 0;
	}
	try {
		Program *program = parseProgram(stdin);
		ProgramInterpreter *pi = new ProgramInterpreter(program);
		auto v = pi->next(Interpreter::Scope::empty.into()).get();
		cout << v.toString() << endl;
	} catch (string s) {
		cerr << "Caught: " << s << endl;
	} catch (util::error *err) {
		cerr << "Caught error: " << err->get_message() << endl;
	}
	return 0;
}

