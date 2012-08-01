#include "parsed.hh"

#include "program.hh"

#include <string>
using namespace std;

ParsedProgram::ParsedProgram(map<string, ParsedPipe *> *pipes, string *rname) : pipes(pipes), rname(rname) {

}

ParsedProgram::~ParsedProgram() {
	for (auto p : *pipes)
		delete p.second;
	delete pipes;
	delete rname;
}

Pipe *ParsedProgram::getPipe(const string &name) {
	return NULL;
}

Program *ParsedProgram::extract(ParsedProgram *prog) {
	return NULL;
}

