#include "program.hh"

#include <vector>
using namespace std;

Block::~Block() {}

Pipe::Type Block::type() { return Type::BLOCK; }

vector<Pipe *> Block::prerequisites() {
	return arguments;
}

