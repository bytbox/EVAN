#include "config.hh"
#include "interp.hh"
#include "program.hh"
#include "util.hh"

#include <iostream>
using namespace std;

int main(int argc, char *argv[]) {
	try {
		Block b1, b2;
		b1.fname = "aList";

		Each e1;
		
		b2.fname = "addOne";
		b2.arguments.push_back(&e1.inner);

		auto i = Interpreter::get(&b1);
		cout << int(i->next(Interpreter::Scope::empty).get()) << endl;
	} catch (error *e) {
		cerr << "uncaught error: " << e->get_message() << endl;
	}
	return 0;
}

