#include "interp.hh"
#include "util.hh"

#include <iostream>
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

	// New outer scope. If we were finished, we're not any more.
	last = outsideScope;
	finished = false;

	// Get the next item from source. If it exists, it'll be ready for the
	// Inner pipe when it looks.
	auto val = source->next(outsideScope);
	if (!val.isDefined()) // we're done
		return maybe<Value>();
	srcVec = val.get().vec();

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
	// At this point, srcVec should be populated, and we're just iterating
	// through it.
	
	if (s.lowIndex() >= outer->srcVec.size())
		return maybe <Value> ();
	return outer->srcVec[s.lowIndex()];
}

