#ifndef INTERP_HH
#define INTERP_HH

#include "program.hh"
#include "util.hh"

#include <map>
#include <vector>
using namespace std;

class Value {
public:
	Value();
	Value(const int);
	Value(const double);
	Value(const Param &);

	operator int();
	operator double();

	enum {BOT, INT, DOUBLE, LIST} type;
	union {
		int i;
		double d;
		vector<Value> *l;
	} value;
};

class InterpreterError : public error {
};

class Interpreter {
public:
	static Interpreter *get(Pipe *);

	virtual maybe<Value> next() = 0;
protected:
	typedef Value (*Function)(vector <Param>, vector <Value>);
	static map<string, Function> functions;
};

class EachInterpreter : public Interpreter {
	Each *each;
public:
	EachInterpreter(Each *);
	virtual maybe<Value> next();
};

class BlockInterpreter : public Interpreter {
	Block *block;
	vector <Interpreter *> arguments;
	bool run = false; // relevant when there are no arguments
public:
	BlockInterpreter(Block *);
	virtual maybe<Value> next();
};

#endif /* !INTERP_HH */

