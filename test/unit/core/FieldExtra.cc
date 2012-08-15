#include "test.hh"

#include "program.hh"
#include "util.hh"
using util::vec;

#include <string>
using namespace std;

#include <cassert>

namespace _FieldExtra {
suite s("FieldExtra", module::get("core"));

class TestExtra1 : public FieldExtra<2> {
	string a, b;
public:
	TestExtra1(string s1, string s2) : a(s1), b(s2) {}

	vec<2, string> toFields() const {
		return {a, b};
	}

	void fromFields(const vec<2, string> &fs) {

	}
};

test t1("toString", s, ([](){
	TestExtra1 te("abc", "def");
	assert(te.toString() == "abc:def");
}));
};
