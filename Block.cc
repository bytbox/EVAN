#include "program.hh"

#include <vector>
using namespace std;

vector<Pipe *> Block::prerequisites() {
	return arguments;
}

