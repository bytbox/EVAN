#include "util.hh"
using util::debug::code_position;

#include <string>
#include <sstream>
using namespace std;

code_position::code_position() : code_position("unknown", -1) {}
code_position::code_position(const string &filename, int line) : filename(filename), line(line) {}

code_position::operator std::string () const {
	ostringstream oss;
	oss << filename << ":" << line;
	return oss.str();
}

