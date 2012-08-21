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
	finished = false;
}

maybe <Value> EachInterpreter::next(Scope outsideScope) {
	if (last.isDefined() && outsideScope == last.get())
		return lastVal;

	// New outer scope. If we were finished, we're not any more.
	last = outsideScope;

	// Get the next item from source. If it exists, it'll be ready for the
	// Inner pipe when it looks.
	auto val = source->next(outsideScope);
	if (!val.isDefined()) // we're done
		return maybe<Value>();
	auto srcLst = val.get().asList();
	srcIt = srcLst.begin();
	srcEnd = srcLst.end();

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
	if (!finished) lastVal = maybe <Value> (collect);
	else lastVal = maybe <Value> ();
	return lastVal;
}

maybe <Value> EachInterpreter::Inner::next(Scope s) {
	if (last.isDefined() && s == last.get())
		return lastVal;

	if (outer->srcIt == outer->srcEnd)
		return maybe <Value> ();

	last = maybe<Scope>(s);
	lastVal = maybe<Value>(*outer->srcIt);
	outer->srcIt++;

	return lastVal;
}

EachInterpreter::Passthrough::Passthrough(Each::Passthrough *pt) {
	passthrough = pt;
	target = Interpreter::get(pt->target);
}

maybe<Value> EachInterpreter::Passthrough::next(Scope s) {
	Scope os = s.outer();
	maybe <Value> val = target->next(os);
	if (!val.isDefined()) {
		// Tell outer: the game is up. No more requests should be
		// served from scope os (probably the current outer scope).
		outer->finished = true;
	}
	return val;
}

