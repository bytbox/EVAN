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
public:
	TypeMismatchError() : user_error("Type mismatch error") {};
};

/*!
 * \brief Represents a static, compile-time parameter passed to a function.
 */
class Param {
public:
	Param();
	Param(const int);
	Param(const double);
	Param(const std::string &);

	operator int() const;
	operator double() const;
	operator std::string() const;
	operator const char *() const;

	/*!
	 * \brief Attempts to represent this parameter as a double.
	 *
	 * This is a little different from the orginary double cast above, in
	 * that if this parameter's underlying type is an int, it will still be
	 * succesfully cast to a double.
	 */
	double asDouble() const;

	enum {INT, FLOAT, STRING} type;
	union {
		int i;
		double d;
		const char *s;
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
	virtual ~Comment();
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
	/*!
	 * \brief The associated comment, or NULL if there is none.
	 */
	Comment *comment = NULL;
public:
	enum Type {BLOCK, EACH, EACH_INNER, EACH_PASSTHROUGH};

	virtual ~Pipe();
	virtual Type type() const = 0; // To avoid needing to use RTTI
	virtual std::vector <Pipe *> prerequisites() const = 0;
	virtual Extra &extra() = 0;

	virtual std::string getComment();
	virtual bool hasComment();
	virtual void setComment(const std::string &);
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
	/*!
	 * \brief Pass a pipe through without iterating on it.
	 */
	class Passthrough : public Pipe {
	public:
		Pipe *target;
		Each **outer;
		Passthrough(Pipe *, Each **);
		virtual ~Passthrough();
		virtual Type type() const;
		virtual std::vector <Pipe *> prerequisites() const;
		virtual Extra &extra();
		NullExtra extraInfo;
	};

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
	Program(Pipe*, std::vector<Comment *>);
	Pipe *result;

	/*!
	 * \brief Top-level comments in this program.
	 */
	std::vector<Comment *> comments;
};

#endif /* !PROGRAM_HH */

