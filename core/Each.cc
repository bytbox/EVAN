#include "program.hh"

#include <vector>
using namespace std;

Each::Each() {
	inner.outer = this;
}

Each::~Each() {}

Pipe::Type Each::type() const {
	return Type::EACH;
}

vector<Pipe *> Each::prerequisites() const {
	return {source};
}

Each::Inner::~Inner() {}

Pipe::Type Each::Inner::type() const {
	return Type::EACH_INNER;
}

vector<Pipe *> Each::Inner::prerequisites() const {
	return {};
}

