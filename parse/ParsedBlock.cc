#include "parsed.hh"

#include "program.hh"

ParsedBlock::ParsedBlock(
		const std::string *blockName,
		std::vector<ParsedParam *> *params,
		std::vector<std::string *> *args)
: blockName(blockName), params(params), args(args)
{}

Block *ParsedBlock::extract(ParsedProgram *) {
	return NULL;
}

