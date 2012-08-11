#include "util.hh"
using namespace util::logging;

#include <ostream>
#include <string>
using namespace std;

stream_log::stream_log(ostream &stream) : stream(stream) {}

void stream_log::output(const std::string &s) {
	stream << s << endl;
}

void stream_log::close() {}

