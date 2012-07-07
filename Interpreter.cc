#include "interp.hh"
#include "util.hh"

#include <map>
using namespace std;

Value justOne(vector <Param> ps, vector <Value> vs) {
	return 1;
}

Value addOne(vector <Param> ps, vector <Value> vs) {
	return int(vs[0]) + 1;
}

const map<string, Interpreter::Function> Interpreter::functions =
	make_map<string, Interpreter::Function>()
	("justOne", justOne)
	("addOne", addOne);


