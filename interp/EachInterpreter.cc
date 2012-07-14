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
	/// \todo handle repeated scopes

	// We want to get a list from this.result, using scopes inside the
	// starting scope.
	auto s = outsideScope.into();
	vector <Value> collect;
	maybe <Value> value;
	do {
		value = result->next(s);
		s = s.next();
	} while(value.isDefined() && (collect.push_back(value.get()), true));
	return maybe <Value> (collect);
}

maybe <Value> EachInterpreter::Inner::next(Scope s) {
	// \todo handle repeated scopes
	return maybe <Value> ();
}

