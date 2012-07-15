#ifndef UTIL_HH
#define UTIL_HH

#include <map>
#include <ostream>
#include <string>
#include <sstream>

/*!
 * \brief Functionality not conceptually specific to EVAN.
 *
 * We follow the STL naming convention (all lower-case with underscores) in
 * this namespace, so that it is visually distinguished from code which is more
 * tightly coupled with EVAN's core logic.
 */
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
	bool isDefined() const { return defined; }
	T get() const {
		if (!defined) throw new internal_error("maybe: get() called on undefined");
		return val;
	}
};

template <typename A, typename B>
class either {
	bool left;
	A leftV;
	B rightV;
public:
	either(A a) : leftV(a), left(true) {}
	either(B b) : rightV(b), left(false) {}
	bool isLeft() const { return left; }
	bool isRight() const { return !left; }
	A getLeft() const {
		if (!left) throw new internal_error("either: getLeft() called on right");
		return leftV;
	}
	B getRight() const {
		if (left) throw new internal_error("either: getRight() called on left");
		return rightV;
	}
};

namespace serial {

/*!
 * \brief A non-composite structure supporting trivial serialization to a
 * variety of forms.
 *
 * Although it is not (cannot be) specified by the interface, implementations
 * should also provide a constructor from std::string, to allow
 * deserialization.
 */
class atom {
public:
	virtual operator std::string() const = 0;
};

template <typename N>
class numeric_atom {
	N n;
public:
	numeric_atom(N n) : n(n) {}
	operator std::string() const {
		using namespace std;
		ostringstream oss;
		oss << n;
		return oss.str();
	}
};

class string_atom {
	const std::string s;
public:
	string_atom(const std::string &s);
	operator std::string() const;
};

/*!
 * \brief A interface for classes providing the necessary hooks for serialization.
 *
 * This definition mostly exists for documentation, in that the typechecker
 * will not verify that type arguments to \ref serializer actually inherit from
 * it. However, as with \ref format below, it is good form to do so.
 */
class serializable {
public:
};

/*!
 * \brief A partial implementation for \ref serializable.
 */
class basic_serializable : public serializable {
public:
};

/*!
 * \brief Interface for backends to \ref serializer.
 *
 * This definition exists entirely for documentation. Although it is good form
 * to inherit it, the C++ type checker will not care if type arguments passed
 * to \ref serializer do so.
 */
class format {
public:
};

class bin_format : public format {
public:
};

class xml_format : public format {
public:
};

class json_format : public format {
public:
};

/*!
 * \brief A serializer for a class implementing the \ref serializable interface.
 */
template <typename serializable, typename format = xml_format>
class serializer {
	const format f;
public:
	serializer() {}
	void write(std::ostream &s);
};

};

}

#endif /* !UTIL_HH */

