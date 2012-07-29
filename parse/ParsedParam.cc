#include "parsed.hh"

ParsedParam::ParsedParam(const int i) : p(i) {}

ParsedParam::ParsedParam(const double d) : p(d) {}

Param ParsedParam::extract(ParsedProgram *) {
	return p;
}

