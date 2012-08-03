#include "interp.hh"
#include "util.hh"

#include <vector>
using namespace std;

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

simple_registry<Interpreter::Function> Interpreter::coreFunctions
({ {"Constant", (IFUNC { return vs[0]; })}
 });
