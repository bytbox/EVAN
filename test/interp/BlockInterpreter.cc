#include "test.hh"

#include "interp.hh"

#include <cassert>

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

void testmain() {
	// Add some functions
	Interpreter::addFunction("test_justOne", (IFUNC { return 1; }));
	Block b;
	b.fname = "test_justOne";
	auto i = Interpreter::get(&b);
	auto s = Interpreter::Scope::empty.into();
	assert(int(i->next(s).get()) == 1);
	assert(int(i->next(s).get()) == 1);
	assert(! i->next(s.next()).isDefined());
}

