#include "interp.hh"

EachInterpreter::EachInterpreter(Each *each) : each(each) {

}

maybe<Value> EachInterpreter::next() {
	return maybe<Value>();
}

