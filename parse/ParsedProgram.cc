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
	pipes[name] = parsed->extract(this);
	return pipes[name];
}

Program *ParsedProgram::extract(ParsedProgram *prog) {
	return new Program(getPipe(*rname));
}

