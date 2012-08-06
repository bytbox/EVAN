#include "util.hh"
using util::error;
using util::debug::code_position;

error *error::with(debug::code_position p) {
	this->position = p;
	return this;
}

