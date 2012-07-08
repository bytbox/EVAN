#include "interp.hh"

#include <vector>
using namespace std;

Interpreter::Scope::Scope() {}
Interpreter::Scope::Scope(vector<int> d) : data(d) {}

const Interpreter::Scope Interpreter::Scope::empty = Scope();

Interpreter::Scope Interpreter::Scope::next() const {
	Scope s(data);
	s.data[s.data.size()-1]++;
	return s;
}

Interpreter::Scope Interpreter::Scope::into() const {
	Scope s(data);
	s.data.push_back(0);
	return s;
}

