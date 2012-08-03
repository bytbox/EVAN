#include "interp.hh"
#include "util.hh"

#include <vector>
using namespace std;

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

simple_registry<Interpreter::Function> *Interpreter::coreFunctions =
new simple_registry<Interpreter::Function>(
		{ {"Constant", (IFUNC { return vs[0]; })}
		});

