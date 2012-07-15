#include "test.hh"

#include <string>
using namespace std;

test::test(const string &name, suite &s) : name(name) {
	s.add(this);
}

void test::run() const {

}

