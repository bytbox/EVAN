#include "program.hh"

#include <functional>
#include <vector>
using namespace std;

Each::Each(Pipe *source, std::function<Pipe* (Pipe*)> resultGen) : source(source) {
	inner.outer = this;
	result = resultGen(&inner);
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

