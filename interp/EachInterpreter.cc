#include "interp.hh"
#include "util.hh"

#include <vector>
using namespace std;

EachInterpreter::EachInterpreter(Each *each) : each(each) {
	cache[each] = this;
	inner.outer = this;
	result = Interpreter::get(each->result);
	source = Interpreter::get(each->source);
}

maybe <Value> EachInterpreter::next(Scope outsideScope) {
	if (last.isDefined() && outsideScope == last.get())
		return lastVal;
	last = outsideScope;

	// We want to get a list from this.result, using scopes inside the
	// starting scope. The decision to terminate is made from Inner (since
	// it's based on what's coming to source).

	auto s = outsideScope.into();
	vector <Value> collect;
	maybe <Value> value;
	do {
		value = result->next(s);
		s = s.next();
	} while(value.isDefined() && (collect.push_back(value.get()), true));
	lastVal = maybe <Value> (collect);
	return lastVal;
}

maybe <Value> EachInterpreter::Inner::next(Scope s) {
	// \todo handle repeated scopes

	Scope outerScope = s.outer();

	return maybe <Value> ();
}

