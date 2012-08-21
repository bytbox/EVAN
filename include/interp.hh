#ifndef INTERP_HH
#define INTERP_HH

#include "foreign.h"
#include "program.hh"
#include "util.hh"

using namespace util;

#include <initializer_list>
#include <list>
#include <map>
#include <string>
#include <vector>

/*!
 * \brief Represents a concrete value acquired during interpretation.
 */
class Value {
public:
	Value();
	Value(const bool);
	Value(const int);
	Value(const float);
	Value(const Param &);
	Value(const std::initializer_list <Value> &);
	Value(const std::vector <Value> &);
	Value(const std::list <Value> &);

	Value(const Bool);
	Value(const Int);
	Value(const Float);

	/*!
	 * \brief Construct an interpreter value encapsulating a foreign
	 * pointer. The underlying value will not be accessible to non-foreign
	 * code.
	 */
	Value(const Foreign);

	Value(const Vec_Bool);
	Value(const Vec_Int);
	Value(const Vec_Float);
	Value(const Vec_Foreign);
	Value(const List_Foreign);

	/*!
	 * \brief Convert the value to a string for debugging purposes.
	 * User-facing code should use the `output` module.
	 */
	std::string toString() const;

	/*!
	 * \throw TypeMismatchError
	 */
	operator int() const;
	/*!
	 * \throw TypeMismatchError
	 */
	operator float() const;
	/*!
	 * \throw TypeMismatchError
	 */
	operator bool() const;

	/*!
	 * \throw TypeMismatchError
	 */
	operator Int() const;
	/*!
	 * \throw TypeMismatchError
	 */
	operator Float() const;
	/*!
	 * \throw TypeMismatchError
	 */
	operator Bool() const;
	/*!
	 * \throw TypeMismatchError
	 */
	operator Foreign() const;
	/*!
	 * \throw TypeMismatchError
	 */
	operator Vec_Int() const;
	/*!
	 * \throw TypeMismatchError
	 */
	operator Vec_Bool() const;
	/*!
	 * \throw TypeMismatchError
	 */
	operator Vec_Float() const;
	/*!
	 * \throw TypeMismatchError
	 */
	operator Vec_Foreign() const;
	/*!
	 * \throw TypeMismatchError
	 */
	operator List_Foreign() const;

	/*!
	 * \brief Access the nth element of the list contained in this value.
	 *
	 * It is an error to call this method on a value with type other than
	 * VEC.
	 *
	 * \throw TypeMismatchError
	 */
	Value operator [](int) const;

	/*!
	 * \throw TypeMismatchError
	 */
	std::vector<Value> vec() const;

	/*!
	 * \throw TypeMismatchError
	 */
	std::list<Value> lst() const;

	/*!
	 * \throw TypeMismatchError
	 */
	double asDouble() const;
	bool isNumeric() const;

	enum {BOT, BOOL, INT, FLOAT, VEC, LIST, FOREIGN} type;
	union {
		bool b;
		int i;
		double d;
		void *f;
	} value;
	maybe < std::vector<Value> > v;
	maybe < std::list<Value> > l;
};

class InterpreterError : public internal_error {
};

class Interpreter {
public:
	/*!
	 * \brief Details the context in which a call to Interpreter::next() is
	 * being made.
	 *
	 * It allows us to tell when to serve the next result versus when we're
	 * still talking about the old one.
	 */
	class Scope {
		std::vector<unsigned int> data;
	public:
		static const Scope empty;

		Scope();
		Scope(std::vector<unsigned int>);

		bool operator==(const Scope &) const;
		bool operator!=(const Scope &) const;

		Scope next() const;
		Scope into() const;
		Scope outer() const;

		/*!
		 * \brief Truncate this scope to that level.
		 */
		Scope level(unsigned int) const;
		unsigned int lowIndex() const;

		operator std::string() const;
	};

	static Interpreter *get(Pipe *);

	virtual maybe<Value> next(Scope) = 0;

protected:
	static std::map<Pipe *, Interpreter *> cache;

	typedef Value (*Function)(std::vector <Param>, std::vector <Value>);
	typedef registry<Function> FunctionRegistry;
	static FunctionRegistry *functions;

	static simple_registry<Function> testFunctions, coreFunctions, combinatoricsFunctions, mathFunctions, foreignFunctions;

#ifdef TESTING
public:
	/*!
	 * \brief Testing hook to provider an interpreted function.
	 *
	 * \testing
	 */
	static void addFunction(const std::string &, Function);
#endif

};

class EachInterpreter : public Interpreter {
	Each *each;
	Interpreter *source, *result;
	maybe <Scope> last;
	maybe <Value> lastVal;
	std::list <Value> ::iterator srcIt;
	std::list <Value> ::iterator srcEnd;
	bool finished;
public:
	/*!
	 * \brief Represents a non-iterated pipe.
	 */
	class Passthrough : public Interpreter {
		Interpreter *target;
		Each::Passthrough *passthrough;
	public:
		EachInterpreter *outer;
		Passthrough(Each::Passthrough *);
		virtual maybe<Value> next(Scope);
	};
	/*!
	 * \brief Represents the pipe used as a source by the first block
	 * inside the Each construct.
	 */
	class Inner : public Interpreter {
		maybe <Scope> last;
		maybe <Value> lastVal;
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

class ProgramInterpreter : public Interpreter {
	Program *program;
	Interpreter *resultInterpreter;
public:
	ProgramInterpreter(Program *);
	virtual maybe<Value> next(Scope);
};

#endif /* !INTERP_HH */

