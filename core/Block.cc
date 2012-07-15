#include "program.hh"

#include <vector>
using namespace std;

Block::Block(const string &fname, vector <Param> params, vector <Pipe *> args) : fname(fname), params(params), arguments(args) {}

Block::~Block() {}

Pipe::Type Block::type() const {
	return Type::BLOCK;
}

vector<Pipe *> Block::prerequisites() const {
	return arguments;
}

