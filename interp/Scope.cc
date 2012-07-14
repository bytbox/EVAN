#include "interp.hh"
#include "util.hh"

#include <vector>
using namespace std;

Interpreter::Scope::Scope() {}
Interpreter::Scope::Scope(vector<int> d) : data(d) {}

const Interpreter::Scope Interpreter::Scope::empty = Scope();

bool Interpreter::Scope::operator==(const Interpreter::Scope &s) const {
	if (data.size() != s.data.size()) return false;
	for (size_t i=0; i < data.size(); i++)
		if (data[i] != s.data[i]) return false;
	return true;
}

bool Interpreter::Scope::operator!=(const Interpreter::Scope &s) const {
	return !(*this == s);
}

Interpreter::Scope Interpreter::Scope::next() const {
	if (data.size() < 1)
		throw new internal_error("attempted to advance empty scope");
	Scope s(data);
	s.data[s.data.size()-1]++;
	return s;
}

Interpreter::Scope Interpreter::Scope::into() const {
	Scope s(data);
	s.data.push_back(0);
	return s;
}

Interpreter::Scope Interpreter::Scope::outer() const {
	Scope s(data);
	s.data.pop_back();
	return s;
}

