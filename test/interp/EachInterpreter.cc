#include "test.hh"

#include "interp.hh"

#include <cassert>

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

void testmain() {
	Interpreter::addFunction("test_aList", (IFUNC { return {1, 3, 5, 7, 9}; }));
	Interpreter::addFunction("test_addOne", (IFUNC { return int(vs[0])+1; }));

	Block start("test_aList", {}, {});
	Each e;
	e.source = &start;

	Block addOne("test_addOne", {}, {&e});
	e.result = &addOne;

	auto i = Interpreter::get(&e);
	auto s = Interpreter::Scope::empty.into();
	auto v = i->next(s).get();
	assert(int(v[0]) == 2);
	assert(int(v[2]) == 6);
	assert(! i->next(s.next()).isDefined());
}

