#include "util.hh"
using namespace util::logging;

#include <string>
using namespace std;

log::log() {
	fmt = new default_format;
}

void log::write(const string &source, const entry &e) {
	output(fmt->entryString(source, e));
}

