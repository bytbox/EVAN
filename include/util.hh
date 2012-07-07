#ifndef UTIL_HH
#define UTIL_HH

#include <map>
#include <string>

namespace util {

class error {
public:
	virtual std::string get_message() = 0;
};

class internal_error : public error {
	std::string message;
public:
	internal_error();
	internal_error(std::string);
	virtual std::string get_message();
};

class user_error : public error {
	std::string message;
public:
	user_error();
	user_error(std::string);
	virtual std::string get_message();
};

template <typename T>
class maybe {
	bool defined;
	T val;
public:
	maybe() : defined(false) {}
	maybe(T v) : defined(true), val(v) {}
	bool isDefined() { return defined; }
	T get() {
		if (!defined) throw new internal_error("maybe: get() called on undefined");
		return val;
	}
};

}

using util::maybe;

using util::error;
using util::internal_error;
using util::user_error;

#endif /* !UTIL_HH */

