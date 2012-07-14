#include "test.hh"

#include "interp.hh"

#include <vector>

#include <cassert>

#define IFUNC [] (std::vector <Param> ps, std::vector <Value> vs) -> Value

void testmain() {
	// Function for testing
	Interpreter::addFunction("test_justOne", (IFUNC { return 1; }));
	Block b("test_justOne", {}, {});
	auto i = Interpreter::get(&b);
	auto s = Interpreter::Scope::empty.into();
	assert(int(i->next(s).get()) == 1);
	assert(int(i->next(s).get()) == 1);
	assert(! i->next(s.next()).isDefined());
}

