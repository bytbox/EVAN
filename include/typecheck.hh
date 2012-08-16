#ifndef TYPECHECK_HH
#define TYPECHECK_HH

#include <string>
#include <vector>

/*!
 * \brief Represents the type of a Value.
 *
 * Functions are not first-class objects, so the type of a function is not a
 * Type.
 */
class Type {
public:
};

class AnyType : public Type {
public:
};

extern AnyType anyType;

/*!
 * \brief A primitive native (or built-in) type.
 */
class PrimitiveType : public Type {
	enum Type {BOOL, INT, FLOAT} type;
	PrimitiveType(Type);
public:
	static PrimitiveType fromString(const std::string &);
	std::string toString() const;
};

class ListType : public Type {
	Type *element;
public:
};

class VecType : public Type {
	Type *element;
public:
};

class ForeignType : public Type {
public:
};

class VariableType : public Type {
public:
};

/*!
 * Because blocks (functions) are not first class objects, BlockType does not
 * implement Type.
 */
class BlockType {
	std::vector <Type *> parameters;
	std::vector <Type *> arguments;
	std::vector <Type *> results;
public:
	BlockType(std::vector <Type *>, std::vector <Type *>, std::vector <Type *>);
};

#endif /* !TYPECHECK_HH */

