#ifndef UTIL_HH
#define UTIL_HH

#include <map>

namespace util {

class error {
public:
};

class internal_error : public error {
public:
};

template <typename K, typename V>
class MapBuilder {
public:
	MapBuilder & operator()(K k, V v) {
		result[k] = v;
		return *this;
	}
	operator std::map <K,V> () {
		return result;
	}
	std::map <K,V> result;
};

template <typename K, typename V>
MapBuilder <K,V> make_map() {
	return MapBuilder <K,V> ();
}

template <typename T>
class maybe {
	bool defined;
	T val;
public:
	maybe() : defined(false) {}
	maybe(T v) : defined(true), val(v) {}
	bool isDefined() { return defined; }
	T get() {
		if (!defined) throw internal_error();
		return val;
	}
};

}

using util::make_map;

using util::maybe;

using util::error;
using util::internal_error;

#endif /* !UTIL_HH */

