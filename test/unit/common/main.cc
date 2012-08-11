#include "test.hh"
#include "util.hh"
using util::error;

#include <iostream>
#include <string>
using namespace std;

#include <cstdlib>
#include <unistd.h>

int main(int argc, char *argv[]) {
	try {
		chdir(argv[1]);
		module::runAll();
	} catch (util::error *err) {
		cerr << "Caught error: " << err->get_message() << endl;
		cerr << "  at: " << string(err->position) << endl;
	}
	return EXIT_SUCCESS;
}

