#include "program.hh"

#include <string>
using namespace std;

string NullExtra::toString() const {
	return "";
}

void NullExtra::fromString(const std::string &s) {
	if (s.size() > 0)
		throw new internal_error("NullExtra given non-empty string");
}
