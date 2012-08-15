#include "parsed.hh"
#include "parse.hh"

#include "program.hh"

#include <iostream>
#include <map>
#include <string>
using namespace std;

ParsedEach::ParsedEach(
		const string *osrc,
		const string *isrc,
		map<string, ParsedPipe *> *parsed_pipes,
		const string *result)
: outer_source(osrc), inner_source(isrc), result(result), parsed_pipes(parsed_pipes), scope(this)
{}

ParsedEach::~ParsedEach() {
	delete outer_source;
	delete inner_source;
	delete result;
	for (auto p : *parsed_pipes)
		delete p.second;
	delete parsed_pipes;
}

ParsedEach::Scope::Scope(ParsedEach *pe) : each(pe) {
	source_name = each->inner_source;
	parsed_pipes = each->parsed_pipes;
}

Pipe *ParsedEach::Scope::getPipe(const string &name) {
	if (name == *source_name) return source;
	if (pipes.find(name) != pipes.end())
		return pipes[name];
	auto parsed = (*parsed_pipes)[name];
	if (!parsed) {
		// Not found in this scope; try the one higher
		return new Each::Passthrough(parent->getPipe(name), &each->extracted);
	}
	pipes[name] = parsed->extract(this);
	return pipes[name];
}

Each *ParsedEach::extract(ParsingScope *prog) {
	// The argument is our parent scope, which will be used by
	// ParsedEach::Scope.
	scope.parent = prog;
	
	// Most of the construction of an each block involves fiddling with the
	// links between inner and outer - but first, we need to get the
	// (outer) source.
	Pipe *source = prog->getPipe(*outer_source);
	extracted = new Each(source, ([this]
				(Pipe *src) -> Pipe * {
					scope.source = src;
					return scope.getPipe(*result);
				}));
	return extracted;
}

