#include "test.hh"

#include "program.hh"

#include <cassert>

namespace _Param {
suite s("Param", module::get("core"));
test t1("ofInt", s, ([](){
	Param p(1);
	assert(int(p) == 1);;
}));
};

