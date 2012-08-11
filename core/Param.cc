#include "program.hh"

#include <string>
using namespace std;

Param::Param() : type(INT) { value.i = 0; }
Param::Param(const int i) : type(INT) { value.i = i; }
Param::Param(const double d) : type(FLOAT) { value.d = d; }
Param::Param(const string &s) : type(STRING) { value.s = s.c_str(); }

Param::operator int() const {
	if (type != INT)
		throw (new TypeMismatchError())->with(_POS);
	return value.i;
}

Param::operator double() const {
	if (type != FLOAT)
		throw (new TypeMismatchError())->with(_POS);
	return value.d;
}

Param::operator string() const {
	if (type != STRING)
		throw (new TypeMismatchError())->with(_POS);
	return value.s;
}

Param::operator const char *() const {
	if (type != STRING)
		throw (new TypeMismatchError())->with(_POS);
	return value.s;
}

