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
 */
class Extra {
public:
};

/*!
 * \brief Blank Extra stub carrying no information.
 */
class NullExtra : public Extra {
public:
};

class BlockExtra : public Extra {
public:
	IVec2 position;
};

class EachExtra : public Extra {
public:
	IVec2 position;
	IVec2 size;
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

