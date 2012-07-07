#include "interp.hh"
#include "program.hh"
#include "util.hh"

#include <iostream>
using namespace std;

int main(int argc, char *argv[]) {
	try {
		Block b1, b2;
		b1.fname = "justOne";
		b2.fname = "addOne";
		b2.arguments.push_back(&b1);

		auto i = Interpreter::get(&b2);
		cout << int(i->next().get()) << endl;
	} catch (error *e) {
		cerr << "Caught error: " << e->get_message() << endl;
	}
	return 0;
}

