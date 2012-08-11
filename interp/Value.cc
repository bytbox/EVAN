#include "interp.hh"

#include <initializer_list>
#include <list>
#include <sstream>
#include <string>
#include <vector>
using namespace std;

Value::Value() : type(BOT) {}
Value::Value(const bool b) : type(BOOL) { value.b = b; }
Value::Value(const int i) : type(INT) { value.i = i; }
Value::Value(const double d) : type(FLOAT) { value.d = d; }
Value::Value(const Param &p) {
	switch (p.type) {
	case Param::INT:
		type = INT;
		value.i = p.value.i;
		break;
	case Param::FLOAT:
		type = FLOAT;
		value.d = p.value.d;
		break;
	case Param::STRING:
		throw (new InterpreterError())->with(_POS);
	default:
		throw (new InterpreterError())->with(_POS);
	}
}
Value::Value(const std::initializer_list <Value> &vs) : type(VEC) {
	v = vector<Value>(vs);
}
Value::Value(const vector <Value> &vs) : type(VEC), v(vs) {}
Value::Value(const list <Value> &vs) : type(LIST), l(vs) {}

string Value::toString() const {
	switch (type) {
	case BOOL:
		return value.b ? "True" : "False";
	case INT:
		return asString<int>(value.i);
	case FLOAT:
		return asString<double>(value.d);
	case BOT:
		return "null";
	case VEC:
		{
			ostringstream oss;
			oss << "<vec>[ ";
			for (Value v : l.get())
				oss << v.toString() << ' ';
			oss << "]";
			return oss.str();
		}
	case LIST:
		{
			ostringstream oss;
			oss << "<list>[ ";
			for (Value v : l.get())
				oss << v.toString() << ' ';
			oss << "]";
			return oss.str();
		}
	default:
		throw (new impossible_error())->with(_POS);
	}
}

Value::operator int() const {
	if (type != INT)
		throw (new TypeMismatchError())->with(_POS);
	return value.i;
}

Value::operator double() const {
	if (type != FLOAT)
		throw (new TypeMismatchError())->with(_POS);
	return value.d;
}

Value::operator bool() const {
	if (type != BOOL)
		throw (new TypeMismatchError())->with(_POS);
	return value.b;
}

Value Value::operator [](int i) const {
	if (type != VEC)
		throw (new TypeMismatchError())->with(_POS);
	return v.get()[i];
}

vector<Value> Value::vec() const {
	if (type != VEC)
		throw (new TypeMismatchError())->with(_POS);
	return vector<Value>(v.get());
}

list<Value> Value::lst() const {
	if (type != LIST)
		throw (new TypeMismatchError())->with(_POS);
	return list<Value>(l.get());
}

