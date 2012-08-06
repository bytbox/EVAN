#include "interp.hh"
#include "util.hh"

#include <iostream>
#include <list>
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
	srcLst = val.get().lst();
	srcIt = srcLst.begin();

	// We want to get a list from this.result, using scopes inside the
	// starting scope. The decision to terminate is made from Inner (since
	// it's based on what's coming to source).

	auto s = outsideScope.into();
	list <Value> collect;
	maybe <Value> value;
	do {
		value = result->next(s);
		s = s.next();
	} while(value.isDefined() && (collect.push_back(value.get()), true));
	lastVal = maybe <Value> (collect);
	return lastVal;
}

maybe <Value> EachInterpreter::Inner::next(Scope s) {
	// At this point, srcLst should be populated, and we're just iterating
	// through it.
	
	if (s.lowIndex() >= outer->srcLst.size())
		return maybe <Value> ();

	if (last.isDefined() && s == last.get())
		return lastVal;

	last = maybe<Scope>(s);
	lastVal = maybe<Value>(*outer->srcIt);
	outer->srcIt++;

	return lastVal;
}

