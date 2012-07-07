#include "interp.hh"

Value::Value() : type(BOT) {}
Value::Value(const int i) : type(INT) { value.i = i; }
Value::Value(const double d) : type(DOUBLE) { value.d = d; }
Value::Value(const Param &p) {
	switch (p.type) {
	case Param::INT:
		type = INT;
		value.i = p.value.i;
		break;
	case Param::DOUBLE:
		type = DOUBLE;
		value.d = p.value.d;
		break;
	default:
		throw new InterpreterError();
	}
}
Value::Value(const initializer_list<Value> &vs) : type(LIST) {
	value.l = new vector<Value>(vs);
}

Value::~Value() {
	if (type == LIST)
		delete value.l;
}

Value::operator int() {
	if (type != INT)
		throw new TypeMismatchError();
	return value.i;
}

Value::operator double() {
	if (type != DOUBLE)
		throw new TypeMismatchError();
	return value.d;
}

