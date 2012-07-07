#include "interp.hh"

Param::Param(const int i) : type(INT) { value.i = i; }
Param::Param(const double d) : type(DOUBLE) { value.d = d; }

Param::operator int() {
	if (type != INT)
		throw new TypeMismatchError();
	return value.i;
}

Param::operator double() {
	if (type != DOUBLE)
		throw new TypeMismatchError();
	return value.d;
}

