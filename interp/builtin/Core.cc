#include "interp.hh"
#include "util.hh"

#include <vector>
using namespace std;

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

simple_registry<Interpreter::Function> Interpreter::coreFunctions
({	{"Identity", (IFUNC { return vs[0]; })},
	{"Constant", (IFUNC { return ps[0]; })},
 });

