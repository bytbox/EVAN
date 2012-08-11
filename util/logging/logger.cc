#include "util.hh"
using namespace util::logging;

#include <map>
#include <vector>
using namespace std;

vector<log *> logger::logs = vector<log *>();
map<std::string, logger *> logger::loggers = map<std::string, logger *>();

logger &logger::get(const std::string &name) {
	auto it = loggers.find(name);
	if (it != loggers.end())
		return *(*it).second;
	loggers[name] = new logger(name);
	return *loggers[name];
}

logger::logger() : logger("misc") {}

logger::logger(const std::string &name) : name(name) {}

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

