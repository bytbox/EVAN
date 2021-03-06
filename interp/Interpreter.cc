#include "interp.hh"
#include "util.hh"

#include <iostream>
#include <map>
using namespace std;

Interpreter *Interpreter::get(Pipe *pipe) {
	// First check to see if we've already encountered this pipe.
	map<Pipe *, Interpreter *>::iterator ir = cache.find(pipe);
	if (ir != cache.end())
		return (*ir).second;

	// We want to avoid introducing a dependency from the program module on
	// the interpreter module, so we manually specialize the interpreter
	// here.

	switch (pipe->type()) {
	case Pipe::BLOCK:
		return new BlockInterpreter((Block *)pipe);
	case Pipe::EACH:
		return new EachInterpreter((Each *)pipe);
	case Pipe::EACH_PASSTHROUGH:
		{
			Each **each = ((Each::Passthrough *)pipe)->outer;
			auto i = new EachInterpreter::Passthrough((Each::Passthrough *)pipe);
			i->outer = (EachInterpreter *)cache[*each];
			return i;
		}
	case Pipe::EACH_INNER:
		{
			Each *e = ((Each::Inner *)pipe)->outer;
			return &((EachInterpreter *)cache[e])->inner;
		}
	}

	throw new internal_error("attempted to create interpreter of unknown type");
}

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

map<Pipe *, Interpreter *> Interpreter::cache;

util::simple_registry<Interpreter::Function> Interpreter::testFunctions;

Interpreter::FunctionRegistry *Interpreter::functions =
new composite_registry<Interpreter::Function>(
		{ &Interpreter::coreFunctions
		, &Interpreter::mathFunctions
		, &Interpreter::combinatoricsFunctions
		, &Interpreter::foreignFunctions
		, &Interpreter::testFunctions
		});

