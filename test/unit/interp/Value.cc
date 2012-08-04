#include "test.hh"

#include "interp.hh"

#include <cassert>

namespace _Value {
SUITE("Value", "interp");
test t1("all", s, ([](){
	assert(int(Value(5)) == 5);
}));
};

