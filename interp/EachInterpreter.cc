#include "interp.hh"
#include "util.hh"

EachInterpreter::EachInterpreter(Each *each) : each(each) {
	inner.outer = this;
}

maybe <Value> EachInterpreter::next(Scope s) {
	return maybe <Value> ();
}

maybe <Value> EachInterpreter::Inner::next(Scope s) {
	return maybe <Value> ();
}

