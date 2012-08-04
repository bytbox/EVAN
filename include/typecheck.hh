#ifndef TYPECHECK_HH
#define TYPECHECK_HH

#include <vector>
using namespace std;

/*!
 * \brief Represents the type of a Value.
 *
 * Functions are not first-class objects, so the type of a function is not a
 * Type.
 */
class Type {
public:
};

/*!
 * \brief A native (or built-in) type.
 */
class NativeType : public Type {
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
};

#endif /* !TYPECHECK_HH */

