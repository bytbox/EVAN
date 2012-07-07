#include "interp.hh"

Value::Value(const int i) : type(INT) { value.i = i; }
Value::Value(const double d) : type(DOUBLE) { value.d = d; }

Value::operator int() {
	if (type != INT)
		throw TypeMismatchException();
	return value.i;
}

Value::operator double() {
	if (type != DOUBLE)
		throw TypeMismatchException();
	return value.d;
}

