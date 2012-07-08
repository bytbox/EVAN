#ifndef PROGRAM_HH
#define PROGRAM_HH

#include "util.hh"

#include <string>
#include <vector>
using namespace std;

/**
 * @brief Indicates that an actual type did not match an expected type.
 *
 * This error can be used during both interpretation and compilation.
 */
class TypeMismatchError : public user_error {
};

/**
 * @brief Represents a static (effectively compile-time) parameter passed to a
 * function.
 */
class Param {
public:
	Param(const int);
	Param(const double);

	operator int();
	operator double();

	enum {INT, DOUBLE} type;
	union {
		int i;
		double d;
	} value;
};

class Pipe {
public:
	virtual vector <Pipe *> prerequisites() = 0;
};

class Block : public Pipe {
public:
	virtual vector<Pipe *> prerequisites();

	string fname;
	vector <Param> params;
	vector <Pipe *> arguments;
};

class Each : public Pipe {
public:
	class Inner : public Pipe {
	public:
		Each *outer;
		virtual vector <Pipe *> prerequisites();
	};

	virtual vector <Pipe *> prerequisites();

	Pipe *source;
	Pipe *result;
	Inner inner;
};

class Program {
public:
	Program(Pipe*);
	Pipe *result;
};

#endif /* !PROGRAM_HH */

