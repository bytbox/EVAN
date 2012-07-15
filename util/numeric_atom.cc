/*!
 * \file util/numeric_atom.cc
 * \brief Specializations of numeric_atom for efficiency
 */

#include "util.hh"

#include <cstdlib>
#include <cstring>

namespace util {

#ifdef itoa
template<>
numeric_atom<int>::operator std::string() const {
	return itoa(n);
}
#elif defined _itoa
template<>
numeric_atom<int>::operator std::string() const {
	return _itoa(n);
}
#endif

};

