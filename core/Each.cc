#include "program.hh"

#include <vector>
using namespace std;

vector<Pipe *> Each::Inner::prerequisites() {
	return {};
}

vector<Pipe *> Each::prerequisites() {
	return {source};
}

