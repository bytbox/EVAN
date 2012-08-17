#ifndef FOREIGNS_HH
#define FOREIGNS_HH

#include "typecheck.hh"

#include <string>
#include <vector>

class ForeignFunc {
public:
	ForeignFunc(const std::string &, const std::string &, const std::string &, const BlockType &);
	const std::string name;
	const std::string cname;
	const std::string description;
	const BlockType type;
};

class ForeignCategory {
public:
	ForeignCategory(const std::string &, const std::string &, const std::vector<ForeignFunc> &);
	const std::string name;
	const std::string cname;
	const std::vector <ForeignFunc> foreignFuncs;
};

class ForeignInfo {
public:
	static const std::vector <ForeignCategory> categories;
};

#endif /* !FOREIGNS_HH */

