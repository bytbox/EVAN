#include "parsed.hh"

ParsedParam::ParsedParam(const int i) : p(i) {}

ParsedParam::ParsedParam(const double d) : p(d) {}

ParsedParam::~ParsedParam() {

}

Param ParsedParam::extract(ParsingScope *) {
	return p;
}

