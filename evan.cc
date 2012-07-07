#include "interp.hh"
#include "program.hh"

#include <iostream>
using namespace std;

int main(int argc, char *argv[]) {
	Block b1, b2;
	b1.fname = "justOne";
	b2.fname = "addOne";
	b2.arguments.push_back(&b1);

	auto i = Interpreter::get(&b1);
	cout << int(i->next().get()) << endl;
	return 0;
}

