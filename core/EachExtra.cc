#include "program.hh"
#include "util.hh"

#include <string>
using namespace std;

vec <4, string> EachExtra::toFields() const {
	return {{
		asString(position.get(0)), asString(position.get(1)),
		asString(size.get(0)), asString(size.get(1)),	
	}};
}

void EachExtra::fromFields(const vec <4, string> &fs) {
	position = {{ofString<int>(fs.get(0)), ofString<int>(fs.get(1))}};
	size = {{ofString<int>(fs.get(2)), ofString<int>(fs.get(3))}};
}

