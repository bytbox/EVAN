#include "parsed.hh"

#include "program.hh"

#include <algorithm>
#include <vector>
using namespace std;

ParsedBlock::ParsedBlock(
		const std::string *blockName,
		vector<ParsedParam *> *params,
		vector<string *> *args)
: blockName(blockName), params(params), args(args)
{}

ParsedBlock::~ParsedBlock() {
	delete blockName;
	for (auto p : *params) delete p;
	delete params;
	for (auto a : *args) delete a;
	delete args;
}

Block *ParsedBlock::extract(ParsingScope *prog) {
	vector<Param> ps(params->size());
	transform(params->begin(), params->end(), ps.begin(),
			([prog](ParsedParam *p) -> Param {
				return p->extract(prog);
			}));
	vector<Pipe *> as(args->size());
	transform(args->begin(), args->end(), as.begin(),
			([prog](string *id) -> Pipe * {
				return prog->getPipe(*id);
			}));
	return new Block(*blockName, ps, as);
}

