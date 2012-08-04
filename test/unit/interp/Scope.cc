#include "test.hh"

#include "interp.hh"

#include <cassert>

namespace _Scope {

suite s("Scope", module::get("interp"));
test t1("basic", s, ([](){
	Interpreter::Scope s = Interpreter::Scope().into();
	assert (s == s);
	assert (s.next() == s.next());
	assert (s.into() == s.into());
	assert (s != s.next());
	assert (s != s.into());
	assert (s.next() != s.into().next());
	assert (s.next().into() != s.into().next());
	assert (s.into().outer() == s);
	assert (s.into().outer() != s.into());
}));

};

