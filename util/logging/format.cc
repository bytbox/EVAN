#include "util.hh"
using namespace util::logging;

#include <string>
using namespace std;

#include <ctime>

string format::timeString(const time_t &tim) const {
	const struct tm *ltime = localtime(&tim);
	const int buflen = 80;
	char buf[buflen];
	strftime(buf, buflen, time_format.c_str(), ltime);
	return buf;
}

string format::levelString(const level &l) const {
	switch(l) {
	case Debug:
		return "DEBUG";
	case Info:
		return "INFO";
	case Warning:
		return "WARNING";
	case Error:
		return "ERROR";
	case Fatal:
		return "FATAL";
	}
	throw (new impossible_error())->with(_POS);
}

