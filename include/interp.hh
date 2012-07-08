#ifndef INTERP_HH
#define INTERP_HH

#include "program.hh"
#include "util.hh"

using namespace util;

#include <initializer_list>
#include <map>
#include <string>
#include <vector>

/**
 * @brief Represents a concrete value acquired during interpretation.
 */
class Value {
public:
	Value();
	Value(const int);
	Value(const double);
	Value(const Param &);
	Value(const initializer_list <Value> &);

	operator int();
	operator double();

	enum {BOT, INT, DOUBLE, LIST} type;
	union {
		int i;
		double d;
	} value;
	maybe < std::vector<Value> > l;
};

class InterpreterError : public internal_error {
};

class Interpreter {
public:
	/**
	 * @brief Details the context in which a call to Interpreter::next() is
	 * being made.
	 *
	 * It allows us to tell when to serve the next result versus when we're
	 * still talking about the old one.
	 */
	class Scope {
		std::vector<int> data;
	public:
		static const Scope empty;

		Scope();
		Scope(std::vector<int>);

		bool operator==(const Scope &) const;
		bool operator!=(const Scope &) const;

		Scope next() const;
		Scope into() const;
	};

	static Interpreter *get(Pipe *);

	virtual maybe<Value> next(Scope) = 0;
protected:
	typedef Value (*Function)(std::vector <Param>, std::vector <Value>);
	static map<std::string, Function> functions;
	static map<Pipe *, Interpreter *> cache;
};

class EachInterpreter : public Interpreter {
	Each *each;
public:
	class Inner : public Interpreter {
	public:
		EachInterpreter *outer;
		virtual maybe<Value> next(Scope);
	};
	EachInterpreter(Each *);
	virtual maybe<Value> next(Scope);
	Inner inner;
};

class BlockInterpreter : public Interpreter {
	Block *block;
	std::vector <Interpreter *> arguments;
	maybe <Scope> last;
	maybe <Value> lastVal;
public:
	BlockInterpreter(Block *);
	virtual maybe<Value> next(Scope);
};

#endif /* !INTERP_HH */

