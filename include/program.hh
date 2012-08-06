#ifndef PROGRAM_HH
#define PROGRAM_HH

#include "util.hh"

using namespace util;

#include <functional>
#include <string>
#include <vector>

/*!
 * \brief Indicates that an actual type did not match an expected type.
 *
 * This error can be used during both interpretation and compilation.
 */
class TypeMismatchError : public user_error {
};

/*!
 * \brief Represents a static, compile-time parameter passed to a function.
 */
class Param {
public:
	Param();
	Param(const int);
	Param(const double);

	operator int() const;
	operator double() const;

	enum {INT, DOUBLE} type;
	union {
		int i;
		double d;
	} value;
};

typedef vec<2, int> IVec2;

/*!
 * \brief Tangential information storage.
 *
 * To allow for more flexible storage and retrieval, we require serialization
 * to be done only via std::string objects, rather than use the full
 * serialization library in libutil.
 */
class Extra {
public:
	virtual std::string toString() const = 0;
	virtual void fromString(const std::string &) = 0;
};

/*!
 * \brief An extra implementation that manages multiple fields.
 */
template <int size, char separator = ':'>
class FieldExtra : public Extra {
public:
	virtual vec<size, std::string> toFields() const = 0;
	virtual void fromFields(const vec<size, std::string> &) = 0;

	std::string toString() const {
		auto fs = toFields();
		std::ostringstream oss;
		for (std::string f : fs.v)
			if (f.find(separator) != std::string::npos)
				throw new internal_error("Separator found in field given to FieldExtra");
			else oss << f << separator;
		auto s = oss.str();
		return s.substr(0, s.size()-1);
	}

	void fromString(const std::string &str) {

	}
};

class Graphical {
public:
};

/*!
 * \brief Blank Extra stub carrying no information.
 */
class NullExtra : public Extra {
public:
	std::string toString() const;
	void fromString(const std::string&);
};

class CommentExtra : public FieldExtra<2> {
public:
	virtual vec<2, std::string> toFields() const;
	virtual void fromFields(const vec<2, std::string> &);

	IVec2 position;
};

class BlockExtra : public FieldExtra<2> {
public:
	virtual vec<2, std::string> toFields() const;
	virtual void fromFields(const vec<2, std::string> &);

	IVec2 position;
};

class EachExtra : public FieldExtra<4> {
public:
	virtual vec<4, std::string> toFields() const;
	virtual void fromFields(const vec<4, std::string> &);

	IVec2 position;
	IVec2 size;
};

class Comment {
public:
	Comment(const std::string &);
	virtual CommentExtra &extra();

	const std::string content;
	CommentExtra extraInfo;
};

/*!
 * \brief
 *
 * Note that this class should not be further subclassed - the kinds of pipes
 * available are hard-coded in several places.
 */
class Pipe {
public:
	enum Type {BLOCK, EACH, EACH_INNER};

	virtual ~Pipe();
	virtual Type type() const = 0; // To avoid needing to use RTTI
	virtual std::vector <Pipe *> prerequisites() const = 0;
	virtual Extra &extra() = 0;
};

class Block : public Pipe {
public:
	Block(const std::string &, std::vector <Param>, std::vector <Pipe *>);
	virtual ~Block();
	virtual Type type() const;
	virtual std::vector <Pipe *> prerequisites() const;
	virtual BlockExtra &extra();

	const std::string fname;
	const std::vector <Param> params;
	const std::vector <Pipe *> arguments;

	BlockExtra extraInfo;
};

/*!
 * \brief An Each construct is a limited foreach loop, with termination
 * guaranteed.
 */
class Each : public Pipe {
public:
	class Inner : public Pipe {
	public:
		Each *outer;
		virtual ~Inner();
		virtual Type type() const;
		virtual std::vector <Pipe *> prerequisites() const;
		virtual Extra &extra();
		NullExtra extraInfo;
	};

	/*!
	 * \brief Create an Each construct.
	 *
	 * The passed function is given a pointer to a Pipe representing the
	 * inner source, and is expected to product a Pipe representing the
	 * inner result.
	 */
	Each(Pipe*, std::function<Pipe* (Pipe*)>);
	virtual ~Each();
	virtual Type type() const;
	virtual std::vector <Pipe *> prerequisites() const;
	virtual EachExtra &extra();

	Pipe *source = NULL;
	Pipe *result = NULL;
	Inner inner;
	EachExtra extraInfo;
};

class Program {
public:
	Program(Pipe*);
	Pipe *result;
};

#endif /* !PROGRAM_HH */

