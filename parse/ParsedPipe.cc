#include "parsed.hh"

ParsedPipe::ParsedPipe(ParsedBlock *b) {
	block = b;
	each = NULL;
}

ParsedPipe::ParsedPipe(ParsedEach *e) {
	block = NULL;
	each = e;
}

ParsedPipe::~ParsedPipe() {
	if (block) delete block;
	if (each) delete each;
}

Pipe *ParsedPipe::extract(ParsedProgram *prog) {
	return NULL;
}

