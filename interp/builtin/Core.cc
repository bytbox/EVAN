#include "interp.hh"
#include "util.hh"

#include <vector>
using namespace std;

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

simple_registry<Interpreter::Function> Interpreter::coreFunctions
({	{"Identity", (IFUNC { return vs[0]; })},
	{"Constant", (IFUNC { return ps[0]; })},
	{"True", (IFUNC { return true; })},
	{"False", (IFUNC { return true; })},
	{"Not", (IFUNC { return !vs[0]; })},
	{"Both", (IFUNC { return vs[0] && vs[1]; })},
	{"Either", (IFUNC { return vs[0] || vs[1]; })},
	{"Neither", (IFUNC { return !(vs[0] || vs[1]); })},
 });

