#include "test.hh"

#include <string>
using namespace std;

test::test(const string &name, suite &s, std::function<void ()> f) : name(name), func(f) {
	s.add(this);
}

void test::run() const {
	func();
}

