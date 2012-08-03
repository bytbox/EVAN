#include "interp.hh"

#include <initializer_list>
#include <vector>
using namespace std;

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
Value::Value(const std::initializer_list <Value> &vs) : type(LIST) {
	l = vector<Value>(vs);
}
Value::Value(const vector <Value> &vs) : type(LIST), l(vs) {}

std::string Value::toString() const {
	switch (type) {
	case INT:
		return asString<int>(value.i);
	case DOUBLE:
		return asString<double>(value.d);
	case BOT:
		return "null";
	case LIST:
		return "<list>"; // TODO
	default:
		throw ""; // TODO
	}
}

Value::operator int() const {
	if (type != INT)
		throw new TypeMismatchError();
	return value.i;
}

Value::operator double() const {
	if (type != DOUBLE)
		throw new TypeMismatchError();
	return value.d;
}

Value Value::operator [](int i) const {
	if (type != LIST)
		throw new TypeMismatchError();
	return l.get()[i];
}

vector<Value> Value::vec() const {
	if (type != LIST)
		throw new TypeMismatchError();
	return vector<Value>(l.get());
}

