#include "util.hh"

namespace util {

string_atom::string_atom(const std::string &s) : s(s) {}

string_atom::operator std::string() const {
	return s; /// \todo quote and escape
}

};

