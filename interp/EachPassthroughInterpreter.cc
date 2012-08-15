#include "interp.hh"
#include "util.hh"

#include <vector>
using namespace std;

EachPassthroughInterpreter::EachPassthroughInterpreter(Each::Passthrough *pt) {
	passthrough = pt;
	target = Interpreter::get(pt->target);
}

maybe<Value> EachPassthroughInterpreter::next(Scope s) {
	return target->next(s.outer());
}

