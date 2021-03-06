#include "parse.hh"
#include "parsed.hh"

#include "program.hh"

#include <string>
using namespace std;

ParsedProgram::ParsedProgram(map<string, ParsedPipe *> *pipes, string *rname) : parsed_pipes(pipes), rname(rname) {

}

ParsedProgram::~ParsedProgram() {
	for (auto p : *parsed_pipes)
		delete p.second;
	delete parsed_pipes;
	delete rname;
}

Pipe *ParsedProgram::getPipe(const string &name) {
	if (pipes.find(name) != pipes.end())
		return pipes[name];
	auto parsed = (*parsed_pipes)[name];
	if (!parsed) throw new ParseError("no such pipe: "+name);
	pipes[name] = parsed->extract(this);
	return pipes[name];
}

Program *ParsedProgram::extract() {
	return new Program(getPipe(*rname));
}

Program *ParsedProgram::extract(ParsingScope *prog) {
	return extract();
}

