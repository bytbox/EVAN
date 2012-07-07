#ifndef INTERP_HH
#define INTERP_HH

#include "program.hh"

#include <map>
#include <vector>
using namespace std;

class Value {
public:
	Value(const int);
	Value(const double);
	Value(const Param &);

	operator int();
	operator double();

	enum {INT, DOUBLE, LIST} type;
	union {
		int i;
		double d;
		vector<Value> *l;
	} value;
};

class InterpreterException {
};

class Interpreter {
	typedef Value (*Function)(vector <Param>, vector <Value>);
	static const map<string, Function> functions;
public:
};

#endif /* !INTERP_HH */

