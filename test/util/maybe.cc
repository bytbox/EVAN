#include "test.hh"

#include "util.hh"
using util::maybe;

#include <cassert>

int main(int argc, char *argv[]) {
	assert(!maybe<int>().isDefined());
	assert(maybe<int>(3).isDefined());
	assert(maybe<int>(3).get() == 3);
	return 0;
}

