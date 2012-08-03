#include "parse.hh"

#include "interp.hh"
#include "program.hh"

#include <iostream>
#include <string>
using namespace std;

#include <cstdio>

int main(int argc, char *argv[]) {
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

