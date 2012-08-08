#include "program.hh"

Param::Param() : type(INT) { value.i = 0; }
Param::Param(const int i) : type(INT) { value.i = i; }
Param::Param(const double d) : type(FLOAT) { value.d = d; }

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

