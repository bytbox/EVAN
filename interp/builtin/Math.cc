#include "interp.hh"
#include "util.hh"

#include <vector>
using namespace std;

#include <cmath>

#define IFUNC [] (vector <Param> ps, vector <Value> vs) -> Value

Value Add(vector <Param> ps, vector <Value> vs) {
	if (vs[0].type == Value::FLOAT || vs[1].type == Value::FLOAT)
		return double(vs[0]) + double(vs[1]);
	else
		return int(vs[0]) + int(vs[1]);
}

Value Sub(vector <Param> ps, vector <Value> vs) {
	if (vs[0].type == Value::FLOAT || vs[1].type == Value::FLOAT)
		return double(vs[0]) - double(vs[1]);
	else
		return int(vs[0]) - int(vs[1]);
}

Value Mul(vector <Param> ps, vector <Value> vs) {
	if (vs[0].type == Value::FLOAT || vs[1].type == Value::FLOAT)
		return double(vs[0]) * double(vs[1]);
	else
		return int(vs[0]) * int(vs[1]);
}

Value Exp(vector <Param> ps, vector <Value> vs) {
	return pow(double(vs[0]), double(vs[1]));
}

Value Div(vector <Param> ps, vector <Value> vs) {
	return double(vs[0]) / double(vs[1]);
}

Value Sum(vector <Param> ps, vector <Value> vs) {
	// Note that all elements in vs[0] will in fact have the same type, so
	// we can decide our behaviour based on the first one.
	list <Value> l = vs[0].lst();
	if (l.size() == 0) return double(0);
	if (l.front().type == Value::INT) {
		int sum = 0;
		for (Value v : l)
			sum += int(v);
		return sum;
	} else {
		double sum = 0;
		for (Value v : l)
			sum += double(v);
		return sum;
	}
}

Value Product(vector <Param> ps, vector <Value> vs) {
	list <Value> l = vs[0].lst();
	if (l.size() == 0) return double(1);
	if (l.front().type == Value::INT) {
		int prod = 1;
		for (Value v : l)
			prod *= int(v);
		return prod;
	} else {
		double prod = 1;
		for (Value v : l)
			prod *= double(v);
		return prod;
	}
}

simple_registry<Interpreter::Function> Interpreter::mathFunctions
({	{"Add", &Add},
	{"Sub", &Sub},
	{"Mul", &Mul},
	{"Div", &Div},
	{"Exp", &Exp},
	{"Sum", &Sum},
	{"Product", &Product},
	{"Lt", (IFUNC { return double(vs[0]) < double(vs[1]); })},
	{"Le", (IFUNC { return double(vs[0]) <= double(vs[1]); })},
	{"Gt", (IFUNC { return double(vs[0]) > double(vs[1]); })},
	{"Ge", (IFUNC { return double(vs[0]) >= double(vs[1]); })},
	{"Eq", (IFUNC { return double(vs[0]) == double(vs[1]); })},
	{"Ne", (IFUNC { return double(vs[0]) != double(vs[1]); })},
 });

