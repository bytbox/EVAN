#include "test.hh"

#include <string>
using namespace std;

module::module(const string &name) : name(name) {

}

void module::add(const suite *s) {
	suites.push_back(s);
}

void module::run() const {

}

