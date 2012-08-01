#include "parsed.hh"

#include "program.hh"

#include <string>
using namespace std;

ParsedProgram::ParsedProgram(map<string, ParsedPipe *> *pipes, string *rname) : pipes(pipes), rname(rname) {

}

Pipe *ParsedProgram::getPipe(const string &name) {
	return NULL;
}

Program *ParsedProgram::extract(ParsedProgram *prog) {
	return NULL;
}

