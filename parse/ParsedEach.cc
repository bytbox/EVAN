#include "parsed.hh"

#include "program.hh"

#include <map>
#include <string>
using namespace std;

ParsedEach::ParsedEach(
		const string *osrc,
		const string *isrc,
		map<string, ParsedPipe *> *pipes,
		const string *result)
: outer_source(osrc), inner_source(isrc), result(result), pipes(pipes)
{}

ParsedEach::~ParsedEach() {
	delete outer_source;
	delete inner_source;
	delete result;
	for (auto p : *pipes)
		delete p.second;
	delete pipes;
}

Each *ParsedEach::extract(ParsedProgram *prog) {
	return NULL; // TODO
}

