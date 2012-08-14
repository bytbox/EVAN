#include "interp.hh"
#include "util.hh"

#include <algorithm>
#include <iostream>
using namespace std;

BlockInterpreter::BlockInterpreter(Block *block) : block(block), arguments(block->arguments.size()) {
	cache[block] = this;
	transform (	block->arguments.begin(),
			block->arguments.end(),
			arguments.begin(),
			&Interpreter::get);
}

maybe<Value> BlockInterpreter::next(Scope s) {
	if (block->arguments.size() == 0) {
		// When there are no arguments, the block is considered to
		// output a single value.

		if (last.isDefined()) return lastVal;

		// We've never run before
		last = maybe<Scope>(s);
		lastVal = maybe<Value>((*functions)[block->fname](block->params, {}));
		return lastVal;
	}

	// If we were called before with the same scope, use that value.
	if (last.isDefined() && s == last.get())
		return lastVal;

	last = maybe<Scope>(s);
	// There is at least one argument - evaluate the sources. The scope
	// will be the same as it was for us.
	vector < maybe <Value> > maybeargs(arguments.size());
	transform(arguments.begin(), arguments.end(), maybeargs.begin(),
			( [s] (Interpreter *i) -> maybe<Value> { return i->next(s); }));

	for (maybe<Value> m : maybeargs)
		if (!m.isDefined()) return maybe<Value>();

	// All values available - call the function.
	vector <Value> args(arguments.size());
	transform(maybeargs.begin(), maybeargs.end(), args.begin(),
			( [] (maybe <Value> m) -> Value { return m.get(); }));
	lastVal = maybe<Value>((*functions)[block->fname](block->params, args));
	return lastVal;
}

