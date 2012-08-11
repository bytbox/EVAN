#include "util.hh"
using namespace util::logging;

#include <map>
#include <vector>
using namespace std;

// This is tricky.
// 
// The initialization order (in which constructors are called) of globals and
// statics is undefined. Since logger::get is commonly called in top-level
// initializers, everything it uses has to be initialized manually. In order to
// pull that off, _loggers must be a pointer.
map<string, logger *> *_loggers = NULL;

logger &logger::get(const std::string &name) {
	if (!_loggers) _loggers = new map<string, logger *>;
	auto it = _loggers->find(name);
	if (it != _loggers->end())
		return *(*it).second;
	logger *nl = new logger(name);
	_loggers->insert(pair<string, logger *>("", nl));
	return *nl;
}

logger &logger::getWith(const std::string &name, std::vector<log *> logs) {
	if (!_loggers) _loggers = new map<string, logger *>;
	auto it = _loggers->find(name);
	if (it != _loggers->end())
		throw (new impossible_error())->with(_POS);
	logger *nl = new logger(name, logs);
	_loggers->insert(pair<string, logger *>("", nl));
	return *nl;
}

logger::logger() : logger("misc") {}

logger::logger(const std::string &name) : name(name) {}

logger::logger(const string &name, vector<log *> logs) : name(name), logs(logs) {}

void logger::logEntry(const entry &e) {
	for (log *l : logs)
		l->write(name, e);
}

void logger::logEntry(const level &lvl, const std::string &msg) {
	logEntry(entry(lvl, msg));
}

void logger::debug(const std::string &msg) {
	logEntry(Debug, msg);
}

void logger::info(const std::string &msg) {
	logEntry(Info, msg);
}

void logger::warning(const std::string &msg) {
	logEntry(Warning, msg);
}

void logger::error(const std::string &msg) {
	logEntry(Error, msg);
}

void logger::fatal(const std::string &msg) {
	logEntry(Fatal, msg);
}

