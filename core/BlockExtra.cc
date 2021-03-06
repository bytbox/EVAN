#include "program.hh"
#include "util.hh"

#include <string>
using namespace std;

vec <2, string> BlockExtra::toFields() const {
	return {asString(position.get(0)), asString(position.get(1))};
}

void BlockExtra::fromFields(const vec <2, string> &fs) {
	position = {ofString<int>(fs.get(0)), ofString<int>(fs.get(1))};
}

