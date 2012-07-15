#include "test.hh"

#include "util.hh"
using util::maybe;

#include <cassert>

namespace _maybe {
suite s("maybe", module::get("interp"));
test t1("basic", s, ([](){
	assert(!maybe<int>().isDefined());
	assert(maybe<int>(3).isDefined());
	assert(maybe<int>(3).get() == 3);
}));
};

