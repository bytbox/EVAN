#include "test.hh"

#include "interp.hh"

#include <cassert>

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

void testmain() {
	// Add some functions
	Interpreter::addFunction("test_justInt", (IFUNC { return 1; }));
}

