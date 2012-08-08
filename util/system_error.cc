#include "util.hh"
using util::system_error;

#include <cerrno>
#include <cstring>
#include <string>
using namespace std;

system_error::system_error() {
	message = "system error: " + string(strerror(errno));
}

system_error::system_error(const std::string &msg) : message("system error: " + msg) {}

string system_error::get_message() {
	return message;
}

