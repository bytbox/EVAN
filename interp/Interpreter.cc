#include "interp.hh"
#include "util.hh"

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

	Block *b = dynamic_cast<Block *>(pipe);	
	if (b) return (cache[pipe] = new BlockInterpreter(b));

	Each *e = dynamic_cast<Each *>(pipe);
	if (e) return (cache[pipe] = new EachInterpreter(e));

	Each::Inner *i = dynamic_cast<Each::Inner *>(pipe);
	if (i) {
		Each *e = i->outer;
		return &(dynamic_cast<EachInterpreter *>(cache[e])->inner);
	}

	throw new internal_error("attempted to create interpreter of unknown pipe");
}

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

map<Pipe *, Interpreter *> Interpreter::cache;

map<string, Interpreter::Function> Interpreter::functions =
{
	{"justOne", (IFUNC { return 1; })},
	{"addOne", (IFUNC { return int(vs[0]) + 1; })},
	{"aList", (IFUNC { return {1, 2, 3}; })},
};


