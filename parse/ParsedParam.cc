#include "parsed.hh"

#include <string>
using namespace std;

ParsedParam::ParsedParam(const int i) : p(i) {}

ParsedParam::ParsedParam(const double d) : p(d) {}

ParsedParam::ParsedParam(const string &s) : p(s) {}

ParsedParam::~ParsedParam() {

}

Param ParsedParam::extract(ParsingScope *) {
	return p;
}

