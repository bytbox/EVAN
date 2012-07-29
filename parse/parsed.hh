#ifndef PARSED_HH
#define PARSED_HH

#include "program.hh"

#include <string>

class ParsedProgram;

template <typename E, typename C = ParsedProgram *>
class ParsedElement {
public:
	virtual E extract(C) = 0;
};

class ParsedComment : ParsedElement<Comment *> {
public:
	explicit ParsedComment(const std::string *);
	virtual Comment *extract(ParsedProgram *);

	const std::string *content;
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

// This class is necessary because C++'s type system isn't expressive enough.
class ParsedPipe : ParsedElement<Pipe *> {
public:
	virtual Pipe *extract(ParsedProgram *);
};

class ParsedProgram : ParsedElement<Program *> {
public:
	virtual Pipe *getPipe(const std::string &);
	virtual Program *extract(Program *);
};

#endif /* !PARSED_HH */

