#ifndef UTIL_HH
#define UTIL_HH

#include <map>

namespace util {

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

}

using util::make_map;

#endif /* !UTIL_HH */

