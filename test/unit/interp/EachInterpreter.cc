#include "test.hh"

#include "interp.hh"

#include <iostream>
#include <list>
#include <vector>

#include <cassert>

#define IFUNC [] (std::vector <Param> ps, std::vector <Value> vs) -> Value

namespace _EachInterpreter {
suite s("EachInterpreter", module::get("interp"));
test t1("basic", s, ([](){
	Interpreter::addFunction("test_aList", (IFUNC { return std::list<Value>{1, 3, 5, 7, 9}; }));
	Interpreter::addFunction("test_addOne", (IFUNC { return int(vs[0])+1; }));

	Block start("test_aList", {}, {});
	Each e(&start, ([] (Pipe *source) -> Pipe * {	
		return new Block("test_addOne", {}, {source});
	}));

	auto i = Interpreter::get(&e);
	auto s = Interpreter::Scope::empty.into();
	auto v = i->next(s);
	assert(v.isDefined());
	auto lst = v.get().lst();
	auto iter = lst.begin();
	assert(int(*iter) == 2);
	iter++;
	assert(int(*iter) == 4);
	assert(i->next(s).isDefined());
}));
};

