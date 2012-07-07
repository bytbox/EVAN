#include "interp.hh"
#include "util.hh"

#include <iostream>
using namespace std;

BlockInterpreter::BlockInterpreter(Block *block) : block(block) {
}

maybe<Value> BlockInterpreter::next() {
	if (block->arguments.size() == 0) {
		if (run) return maybe<Value>();
		run = true;
		return maybe<Value>(functions[block->fname]({}, {}));
	}
	return maybe<Value>();
}

