#include "program.hh"

#include <vector>
using namespace std;

Each::Each() {
	inner.outer = this;
}

vector<Pipe *> Each::Inner::prerequisites() {
	return {};
}

vector<Pipe *> Each::prerequisites() {
	return {source};
}

