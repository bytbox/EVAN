#include "interp.hh"
#include "util.hh"

#include <vector>
using namespace std;

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

Value Add(vector <Param> ps, vector <Value> vs) {
	if (vs[0].type == Value::DOUBLE || vs[1].type == Value::DOUBLE)
		return double(vs[0]) + double(vs[1]);
	else
		return int(vs[0]) + int(vs[1]);
}

Value Sub(vector <Param> ps, vector <Value> vs) {
	if (vs[0].type == Value::DOUBLE || vs[1].type == Value::DOUBLE)
		return double(vs[0]) - double(vs[1]);
	else
		return int(vs[0]) - int(vs[1]);
}

Value Mul(vector <Param> ps, vector <Value> vs) {
	if (vs[0].type == Value::DOUBLE || vs[1].type == Value::DOUBLE)
		return double(vs[0]) * double(vs[1]);
	else
		return int(vs[0]) * int(vs[1]);
}

Value Div(vector <Param> ps, vector <Value> vs) {
	return double(vs[0]) / double(vs[1]);
}

simple_registry<Interpreter::Function> Interpreter::mathFunctions
({	{"Add", &Add},
	{"Sub", &Sub},
	{"Mul", &Mul},
	{"Div", &Div},
	{"Lt", (IFUNC { return double(vs[0]) < double(vs[1]); })},
	{"Le", (IFUNC { return double(vs[0]) <= double(vs[1]); })},
	{"Gt", (IFUNC { return double(vs[0]) > double(vs[1]); })},
	{"Ge", (IFUNC { return double(vs[0]) >= double(vs[1]); })},
	{"Eq", (IFUNC { return double(vs[0]) == double(vs[1]); })},
	{"Ne", (IFUNC { return double(vs[0]) != double(vs[1]); })},
 });

