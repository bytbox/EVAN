#include "interp.hh"
#include "util.hh"

#include <map>
using namespace std;

Interpreter *Interpreter::get(Pipe *pipe) {
	// We want to avoid introducing a dependency from the program module on
	// the interpreter module, so we manually specialize the interpreter
	// here.

	Block *b = dynamic_cast<Block *>(pipe);	
	if (b) return new BlockInterpreter(b);

	Each *e = dynamic_cast<Each *>(pipe);
	if (e) return new EachInterpreter(e);

	throw new internal_error("attempted to create interpreter of unknown pipe");
}

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

map<string, Interpreter::Function> Interpreter::functions =
{
	{"justOne", (IFUNC { return 1; })},
	{"addOne", (IFUNC { return int(vs[0]) + 1; })},
	{"aList", (IFUNC { return {1, 2, 3}; })},
};


