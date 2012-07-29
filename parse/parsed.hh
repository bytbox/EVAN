#ifndef PARSED_HH
#define PARSED_HH

#include "program.hh"

class ParsedProgram;

template <typename E, typename C = ParsedProgram *>
class ParsedElement {
public:
	virtual E extract(C) = 0;
};

class ParsedParam : ParsedElement<Param> {
	Param p;
public:
	explicit ParsedParam(const int);
	explicit ParsedParam(const double);
	
	virtual Param extract(ParsedProgram *);
};

class ParsedBlock : ParsedElement<Block *> {
	const std::string *blockName;
	std::vector<ParsedParam *> *params;
	std::vector<std::string *> *args;
public:
	ParsedBlock(	const std::string *,
			std::vector<ParsedParam *> *,
			std::vector<std::string *> *);
	virtual Block *extract(ParsedProgram *);
};

class ParsedEach : ParsedElement<Each *> {
public:
	virtual Each *extract(ParsedProgram *);
};

class ParsedProgram : ParsedElement<Program *> {
public:
	virtual Program *extract();
};

#endif /* !PARSED_HH */

