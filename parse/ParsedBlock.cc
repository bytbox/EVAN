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

Block *ParsedBlock::extract(ParsedProgram *prog) {
	vector<Param> ps;
	transform(params->begin(), params->end(), ps.begin(),
			([prog](ParsedParam *p) -> Param {
				return p->extract(prog);
			}));
	vector<Pipe *> as;
	transform(args->begin(), args->end(), as.begin(),
			([prog](string *id) -> Pipe * {
				return prog->getPipe(*id);
			}));
	return new Block(*blockName, ps, as);
}

