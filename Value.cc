#include "interp.hh"

Value::Value() : type(BOT) {}
Value::Value(const int i) : type(INT) { value.i = i; }
Value::Value(const double d) : type(DOUBLE) { value.d = d; }

Value::operator int() {
	if (type != INT)
		throw TypeMismatchError();
	return value.i;
}

Value::operator double() {
	if (type != DOUBLE)
		throw TypeMismatchError();
	return value.d;
}

