#include "test.hh"

#include "interp.hh"

#include <vector>

#include <cassert>

#define IFUNC [] (std::vector <Param> ps, std::vector <Value> vs) -> Value

void testmain() {
	Interpreter::addFunction("test_aList", (IFUNC { return {1, 3, 5, 7, 9}; }));
	Interpreter::addFunction("test_addOne", (IFUNC { return int(vs[0])+1; }));

	Block start("test_aList", {}, {});
	Each e(&start, ([] (Pipe *source) -> Pipe * {	
		return new Block("test_addOne", {}, {source});
	}));

	//e.result = &addOne;

	auto i = Interpreter::get(&e);
	auto s = Interpreter::Scope::empty.into();
	auto v = i->next(s);
	assert(v.isDefined());
	//assert(int(v.get()[0]) == 2);
	//assert(int(v.get()[2]) == 6);
	assert(i->next(s).isDefined());
	//assert(int(i->next(s).get()[1]) == 4);
	//assert(! i->next(s.next()).isDefined());

	// add some OO-ness to tests
}

