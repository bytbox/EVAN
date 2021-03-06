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

Pipe *ParsedPipe::extract(ParsingScope *prog) {
	if (block)
		return block->extract(prog);
	else return each->extract(prog);
}

