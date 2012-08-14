#include "interp.hh"
#include "util.hh"

#include <vector>
using namespace std;

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

Value Sequence(vector <Param> ps, vector <Value> vs) {
	list <Value> lst;
	for (int i = as[0]; i < int(as[0]) + int(as[1]); i++)
		lst.push_back(i);
	return lst;
}

simple_registry<Interpreter::Function> Interpreter::coreFunctions
({	{"Identity", (IFUNC { return vs[0]; })},
	{"Constant", (IFUNC { return ps[0]; })},
	{"True", (IFUNC { return true; })},
	{"False", (IFUNC { return true; })},
	{"Not", (IFUNC { return !vs[0]; })},
	{"Both", (IFUNC { return vs[0] && vs[1]; })},
	{"Either", (IFUNC { return vs[0] || vs[1]; })},
	{"Neither", (IFUNC { return !(vs[0] || vs[1]); })},
	{"Count", (IFUNC { return int(vs[0].lst().size()); })},
	{"Sequence", &Sequence},
 });

