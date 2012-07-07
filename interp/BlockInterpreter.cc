#include "interp.hh"
#include "util.hh"

#include <algorithm>
#include <iostream>
using namespace std;

BlockInterpreter::BlockInterpreter(Block *block) : block(block), arguments(block->arguments.size()) {
	transform (	block->arguments.begin(),
			block->arguments.end(),
			arguments.begin(),
			&Interpreter::get);
}

maybe<Value> BlockInterpreter::next() {
	// TODO we could do with some massive optimization here
	if (block->arguments.size() == 0) {
		// When there are no arguments, the block is considered to
		// output a single value.
		if (run) return maybe<Value>();
		run = true;
		return maybe<Value>(functions[block->fname](block->params, {}));
	}
	// There is at least one argument.
	vector < maybe <Value> > maybeargs(arguments.size());
	transform(arguments.begin(), arguments.end(), maybeargs.begin(),
			( [] (Interpreter *i) -> maybe<Value> { return i->next(); }));

	// The 'defined' state of all arguments must match.
	vector <bool> defined(arguments.size());
	transform(maybeargs.begin(), maybeargs.end(), defined.begin(), 
			( [] (maybe <Value> m) -> bool { return m.isDefined(); }));
	bool def = defined[0];
	for (size_t i=1; i<defined.size(); i++)
		if (defined[i] != def) throw new TypeMismatchError();

	if (!def) return maybe<Value>();
	
	vector <Value> args(arguments.size());
	transform(maybeargs.begin(), maybeargs.end(), args.begin(),
			( [] (maybe <Value> m) -> Value { return m.get(); }));
	return maybe<Value>(functions[block->fname](block->params, args));
}

