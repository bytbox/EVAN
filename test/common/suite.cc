#include "test.hh"

#include <string>
using namespace std;

suite::suite(const string &name) : suite(name, NULL) {}

suite::suite(const string &name, module *m) : name(name) {
	if (m) m->add(this);
}

void suite::add(const test *t) {
	tests.push_back(t);
}

void suite::run() const {

}

