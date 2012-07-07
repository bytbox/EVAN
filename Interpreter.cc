#include "interp.hh"
#include "util.hh"

#include <map>
using namespace std;

const map<string, Interpreter::Function> Interpreter::functions =
	make_map<string, Interpreter::Function>()
	("justOne", ([] (vector <Param> ps, vector <Value> vs) -> Value {
		return 1;
	}))
	("addOne", ([] (vector <Param> ps, vector <Value> vs) -> Value {
		return int(vs[0]) + 1;
	}));


