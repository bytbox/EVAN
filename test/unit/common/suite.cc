#include "test.hh"

#include <iostream>
#include <string>
using namespace std;

suite::suite(const string &name) : name(name) {}

suite::suite(const string &name, module &m) : name(name) {
	m.add(this);
}

void suite::add(const test *t) {
	tests.push_back(t);
}

void suite::run() const {
	for (const test *t : tests) {
		t->run();
		cerr << "." << flush;
	}
}

