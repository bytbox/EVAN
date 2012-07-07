#ifndef PROGRAM_HH
#define PROGRAM_HH

#include <map>
#include <string>
#include <vector>
using namespace std;

class Pipe {
public:
};

class Param {
public:
	enum {INT, DOUBLE} type;
	union {
		int i;
		double d;
	} value;
};

class Scope {
public:
	Scope();
	map <string, Pipe> defs;
};

// A link connects pipes across scopes.
class Link {
public:
	string top, bot;
};

class Block : public Pipe {
public:
	string fname;
	vector <Param> params;
	vector <string> arguments;
	Scope &scope;
};

class Each : public Pipe {
public:
	Link source;
	vector <Link> outputs;
	Scope &outer;
	Scope &inner;
};

class Program {
public:
	Program(string, Scope &);
	string result;
	Scope &global;
};

#endif /* !PROGRAM_HH */

