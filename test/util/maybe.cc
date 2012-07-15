#include "test.hh"

#include "util.hh"
using util::maybe;

#include <cassert>

void testmain() {
	assert(!maybe<int>().isDefined());
	assert(maybe<int>(3).isDefined());
	assert(maybe<int>(3).get() == 3);
}

