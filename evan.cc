#include "interp.hh"
#include "program.hh"

#include <iostream>
using namespace std;

int main(int argc, char *argv[]) {
	Block b1, b2;
	b1.fname = "justOne";
	b2.fname = "addOne";
	b2.arguments[0] = &b1;
	return 0;
}

