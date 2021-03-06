#include "foreign.h"
#include "interp.hh"
#include "program.hh"

#include <initializer_list>
#include <list>
#include <sstream>
#include <string>
#include <vector>
using namespace std;

Value::Value() : type(BOT) {}
Value::Value(const bool b) : type(BOOL) { value.b = b; }
Value::Value(const int i) : type(INT) { value.i = i; }
Value::Value(const float d) : type(FLOAT) { value.d = d; }
Value::Value(const Bool b) : type(BOOL) { value.b = b; }
Value::Value(const Int i) : type(INT) { value.i = i; }
Value::Value(const Float d) : type(FLOAT) { value.d = d; }
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

Value::Value(const Foreign f) : type(FOREIGN) { value.f = f; }
Value::Value(const Vec_Int vi) : type(VEC) {
	auto res = vector<Value>{};
	for (unsigned int i = 0; i < vi.len; i++)
		res.push_back(vi.data[i]);
	v = res;
}
Value::Value(const Vec_Float vi) : type(VEC) {
	auto res = vector<Value>{};
	for (unsigned int i = 0; i < vi.len; i++)
		res.push_back(vi.data[i]);
	v = res;
}
Value::Value(const Vec_Bool vi) : type(VEC) {
	auto res = vector<Value>{};
	for (unsigned int i = 0; i < vi.len; i++)
		res.push_back(vi.data[i]);
	v = res;
}
Value::Value(const Vec_Foreign vi) : type(VEC) {
	auto res = vector<Value>{};
	for (unsigned int i = 0; i < vi.len; i++)
		res.push_back(vi.data[i]);
	v = res;
}
Value::Value(const List_Foreign fl) : type(LIST) {
	auto res = list<Value>{};
	Foreign n = foreign_list_next(fl);
	while (n) {
		res.push_back(n);
		n = foreign_list_next(fl);
	}
	l = res;
}

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
			for (Value val : v.get())
				oss << val.toString() << ' ';
			oss << "]";
			return oss.str();
		}
	case LIST:
		{
			ostringstream oss;
			oss << "<list>[ ";
			for (Value val : l.get())
				oss << val.toString() << ' ';
			oss << "]";
			return oss.str();
		}
	case FOREIGN:
		return "<foreign>";
	default:
		throw (new impossible_error())->with(_POS);
	}
}

Value::operator int() const {
	if (type != INT)
		throw (new TypeMismatchError())->with(_POS);
	return value.i;
}

Value::operator float() const {
	if (type != FLOAT)
		throw (new TypeMismatchError())->with(_POS);
	return value.d;
}

Value::operator bool() const {
	if (type != BOOL)
		throw (new TypeMismatchError())->with(_POS);
	return value.b;
}

Value::operator Int() const {
	if (type != INT)
		throw (new TypeMismatchError())->with(_POS);
	return value.i;
}

Value::operator Float() const {
	if (type != FLOAT)
		throw (new TypeMismatchError())->with(_POS);
	return value.d;
}

Value::operator Bool() const {
	if (type != BOOL)
		throw (new TypeMismatchError())->with(_POS);
	return value.b;
}

Value::operator Foreign() const {
	if (type != FOREIGN)
		throw (new TypeMismatchError())->with(_POS);
	return value.f;
}

Value::operator Vec_Int() const {
	if (type != VEC)
		throw (new TypeMismatchError())->with(_POS);
	Vec_Int r;
	r.len = v.get().size();
	for (unsigned int i = 0; i < r.len; i++)
		r.data[i] = v.get()[i];
	return r;
}

Value::operator Vec_Bool() const {
	if (type != VEC)
		throw (new TypeMismatchError())->with(_POS);
	Vec_Bool r;
	r.len = v.get().size();
	for (unsigned int i = 0; i < r.len; i++)
		r.data[i] = v.get()[i];
	return r;
}

Value::operator Vec_Float() const {
	if (type != VEC)
		throw (new TypeMismatchError())->with(_POS);
	Vec_Float r;
	r.len = v.get().size();
	for (unsigned int i = 0; i < r.len; i++)
		r.data[i] = v.get()[i];
	return r;
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

list<Value> Value::asList() const {
	switch (type) {
	case LIST:
		return list<Value>(l.get());
	case VEC:
		{
			list<Value> rl;
			for (Value val : v.get())
				rl.push_back(val);
			return rl;
		}
	
	case BOT:
	case BOOL:
	case INT:
	case FLOAT:
	case FOREIGN:
	default:
		throw (new TypeMismatchError())->with(_POS);
	}
}

double Value::asDouble() const {
	if (type == FLOAT)
		return value.d;
	else if (type == INT)
		return double(value.i);
	else throw (new TypeMismatchError())->with(_POS);
}

bool Value::isNumeric() const {
	return type == INT || type == FLOAT;
}

