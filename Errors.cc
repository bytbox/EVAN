#include "util.hh"

#include <string>
using namespace std;

internal_error::internal_error() : message("unknown internal error") {}

internal_error::internal_error(string msg) : message("internal error: " + msg) {}

string internal_error::get_message() {
	return message;
}

user_error::user_error() : message("unknown user error") {}

user_error::user_error(string msg) : message("user error: " + msg) {}

string user_error::get_message() {
	return message;
}

