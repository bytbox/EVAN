#include "test.hh"

#include "util.hh"
using util::asString;
using util::ofString;

#include <cassert>

namespace _strconv {
suite s("strconv", module::get("util"));
test t1("int2string", s, ([](){
	assert(asString(1) == "1");
	assert(asString(0) == "0");
	assert(asString(105) == "105");
}));
};
