#ifndef PROGRAM_HH
#define PROGRAM_HH

#include <string>
#include <vector>
using namespace std;

class TypeMismatchException {
};

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
};

class Block : public Pipe {
public:
	string fname;
	vector <Param> params;
	vector <Pipe *> arguments;
};

class Each : public Pipe {
	class Inner : public Pipe {
		Each *outer;
	};
public:
	Pipe *source;
	Pipe *result;
	Inner *inner;
};

class Program {
public:
	Program(Pipe*);
	Pipe *result;
};

#endif /* !PROGRAM_HH */

