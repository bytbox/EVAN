#include "util.hh"
using namespace util::logging;

#include <string>
using namespace std;

#include <ctime>

entry::entry(const level &lvl, const std::string &msg)
	: lvl(lvl), message(msg), tm(time(NULL)) {}

