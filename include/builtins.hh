#ifndef BUILTINS_HH
#define BUILTINS_HH

#include <string>
#include <vector>

/*!
 * \brief Description of a builtin block.
 *
 * Note that this class doesn't contain the actual implementation - that's up
 * to the compiler and interpreter modules (etc.) to take care of.
 */
class Builtin {
public:
	Builtin(const std::string &name);
	const std::string name;
};

/*!
 * \brief A category of builtins.
 *
 * Note that although most categories are distinguished by conceptual purpose,
 * some will be linked to the nature of the underlying implementation,
 * especially ones that use external libraries (as users will want to look for
 * familiar features in familiar places.
 */
class Category {
public:
	Category(const std::string &, const std::vector<Builtin> &);
	const std::string name;
	const std::vector <Builtin> builtins;
};

class BuiltinInfo {
public:
	static const std::vector <Category> categories;
};

#endif /* !BUILTINS_HH */

