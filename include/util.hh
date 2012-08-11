#ifndef UTIL_HH
#define UTIL_HH

#include <map>
#include <ostream>
#include <string>
#include <sstream>
#include <vector>

#include <ctime>

#define _POS (debug::code_position(__FILE__, __LINE__))

/*!
 * \brief Functionality not conceptually specific to EVAN.
 *
 * We follow the STL naming convention (all lower-case with underscores) in
 * this namespace, so that it is visually distinguished from code which is more
 * tightly coupled with EVAN's core logic.
 */
namespace util {

namespace debug {
class code_position {
public:
	code_position();
	code_position(const std::string &, int);
	operator std::string () const;
	std::string filename;
	int line;
};
};

/*!
 * \brief Logging utilities.
 *
 * Done properly, a good logging system doubles as a debugging and development
 * tool.
 */
namespace logging {

enum level {Debug, Info, Warning, Error, Fatal};

class entry {
public:
	const level lvl;
	const std::string message;
	const time_t tm;
	entry(const level &, const std::string &);
};

class format {
public:
	virtual std::string timeString(const time_t &) const;
	virtual std::string levelString(const level &) const;
	virtual std::string entryString(const std::string &, const entry &) const = 0;
protected:
	std::string time_format = "%Y-%m-%d %H:%M:%S";
};

class default_format : public format {
public:
	virtual std::string entryString(const std::string &, const entry &) const;
};

class log {
public:
	log();
	virtual void write(const std::string &, const entry &);

	virtual void output(const std::string &) = 0;
	virtual void close() = 0;
protected:
	format *fmt;
};

class stream_log : public log {
public:
	stream_log(std::ostream &stream);
	virtual void output(const std::string &);
	virtual void close();
protected:
	std::ostream &stream;
};

class filestream_log : public stream_log {
public:
	filestream_log(std::ofstream &fstream);
	virtual void close();
protected:
	std::ofstream &fstream;
};

class console_log : public stream_log {
public:
	console_log();
};

class logger {
	static std::vector<log *> logs;
	static std::map<std::string, logger *> loggers;

	const std::string name;
public:
	static logger &get(const std::string &);

	logger();
	logger(const std::string &);

	void logEntry(const entry &);
	void logEntry(const level &, const std::string &);

	/*!
	 * \brief Log a debugging message.
	 *
	 * This is a convenience method for logEntry(level.Debug, msg).
	 */
	void debug(const std::string &);

	/*!
	 * \brief Log an informational message.
	 *
	 * This is a convenience method for logEntry(level.Info, msg).
	 */
	void info(const std::string &);
	
	/*!
	 * \brief Log a warning message.
	 *
	 * This is a convenience method for logEntry(level.Warning, msg).
	 */
	void warning(const std::string &);

	/*!
	 * \brief Log an error message.
	 *
	 * This is a convenience method for logEntry(level.Error, msg).
	 */
	void error(const std::string &);

	/*!
	 * \brief Log a fatal message.
	 *
	 * This is a convenience method for logEntry(level.Fatal, msg). Note
	 * that calling this method will *not* terminate the program (or close
	 * the logger) - the caller must do that itself.
	 */
	void fatal(const std::string &);
};

};

class error {
public:
	debug::code_position position;
	virtual std::string get_message() = 0;

	/*!
	 * \brief Adds the given code_position to the error, and returns the
	 * result.
	 *
	 * The original error object is modified - the result is returned only
	 * as a convenience to the caller.
	 */
	virtual error *with(debug::code_position);
};

class system_error : public error {
	std::string message;
public:
	system_error();
	system_error(const std::string &msg);
	virtual std::string get_message();
};

class internal_error : public error {
	std::string message;
public:
	internal_error();
	internal_error(std::string);
	virtual std::string get_message();
};

/**
 * \todo Add various types (switch, etc.)
 */
class impossible_error : public internal_error {
public:
	impossible_error();
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

template <int L, typename T>
class vec {
public:
	T &operator[](int i) {
		return v[i];
	}
	const T &get(int i) const {
		return v[i];
	}
	T v[L];
};

template <typename T>
std::string asString(T x) {
	std::ostringstream oss;
	oss << x;
	return oss.str();
}

template <typename T>
T ofString(const std::string &s) {
	T x;
	std::istringstream iss(s);
	iss >> x;
	return x;
}

template <typename T>
class registry {
public:
	virtual bool has(const std::string &) const = 0;
	virtual T &operator[](const std::string &) = 0;
};

template <typename T>
class simple_registry : public registry<T> {
	std::map<std::string, T> m;
public:
	simple_registry() {}
	simple_registry(std::map<std::string, T> m) : m(m) {}
	virtual bool has(const std::string &s) const {
		return m.find(s) != m.end();
	}
	virtual T &operator[](const std::string &s) {
		auto i = m.find(s);
		if (i != m.end()) return (*i).second;
		throw new internal_error("not found in simple_registry: "+s);
	}
	virtual void add(const std::string &s, T &t) {
		m[s] = t;
	}
};

/**
 * \brief A registry that performs lookups sequentially through several
 * sub-registries.
 */
template <typename T>
class composite_registry : public registry<T> {
	std::vector<registry<T> *> registries;
public:
	composite_registry(std::vector<registry<T> *> v) : registries(v) {}
	virtual bool has(const std::string &s) const {
		for (registry<T> *r : registries)
			if (r->has(s)) return true;
		return false;
	}
	virtual T &operator[](const std::string &s) {
		for (registry<T> *r : registries)
			if (r->has(s)) return (*r)[s];
		throw new internal_error("not found in composite_registry: "+s);
	}
};

class cli_arguments {
	std::vector<std::string> command_line, arguments, options;
	void process();
public:
	cli_arguments(int, char *[]);
	cli_arguments(std::vector<std::string>);
	bool flag(const std::string &);
	std::string opt(const std::string &);
	std::string opt(const std::string &, const std::string &);
	std::vector<std::string> args();
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
	serializable();
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

