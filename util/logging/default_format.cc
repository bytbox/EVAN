#include "util.hh"
using namespace util::logging;

#include <sstream>
#include <string>
using namespace std;

string default_format::entryString(const string &source, const entry &e) const {
	ostringstream oss;
	oss << "[" << timeString(e.tm) << "] [" << source << "] ";
	oss << levelString(e.lvl) << " : " << e.message;
	return oss.str();
}

