#ifndef PARSED_HH
#define PARSED_HH

#include "program.hh"

#include <string>

class ParsedProgram;

class ParsingScope {
public:
	virtual Pipe *getPipe(const std::string &) = 0;
};

template <typename E, typename C = ParsingScope *>
class ParsedElement {
public:
	virtual ~ParsedElement() {};
	virtual E extract(C) = 0;
};

class ParsedComment : public ParsedElement<Comment *> {
public:
	explicit ParsedComment(const std::string *);
	virtual ~ParsedComment();
	virtual Comment *extract(ParsingScope *);

	const std::string *content;
};

class ParsedParam : public ParsedElement<Param> {
	Param p;
public:
	explicit ParsedParam(const int);
	explicit ParsedParam(const double);
	virtual ~ParsedParam();
	
	virtual Param extract(ParsingScope *);
};

class ParsedPipe;

class ParsedBlock : public ParsedElement<Block *> {
	const std::string *blockName;
	std::vector<ParsedParam *> *params;
	std::vector<std::string *> *args;
public:
	ParsedBlock(	const std::string *,
			std::vector<ParsedParam *> *,
			std::vector<std::string *> *);
	virtual ~ParsedBlock();
	virtual Block *extract(ParsingScope *);
};

class ParsedEach : public ParsedElement<Each *> {
	const std::string *outer_source, *inner_source, *result;
	std::map<std::string, ParsedPipe *> *parsed_pipes;
	Each *extracted;

	class Scope : public ParsingScope {
		std::map<std::string, ParsedPipe *> *parsed_pipes;
		std::map<std::string, Pipe *> pipes;
		const std::string *source_name;
		ParsedEach *each;
	public:
		Scope(ParsedEach *);
		virtual Pipe *getPipe(const std::string &);
		Pipe *source;
		ParsingScope *parent;
	} scope;

public:
	ParsedEach(	const std::string *,
			const std::string *,
			std::map<std::string, ParsedPipe *> *,
			const std::string *);
	~ParsedEach();
	virtual Each *extract(ParsingScope *);
};

// This class is necessary because C++'s type system isn't expressive enough.
class ParsedPipe : public ParsedElement<Pipe *> {
	ParsedBlock *block;
	ParsedEach *each;
public:
	ParsedPipe(ParsedBlock *);
	ParsedPipe(ParsedEach *);
	virtual ~ParsedPipe();
	virtual Pipe *extract(ParsingScope *);
};

class ParsedProgram : public ParsedElement<Program *>, public ParsingScope {
	std::map<std::string, ParsedPipe *> *parsed_pipes;
	std::map<std::string, Pipe *> pipes;
	std::string *rname;
public:
	ParsedProgram(std::map<std::string, ParsedPipe *> *, std::string *);
	virtual ~ParsedProgram();
	virtual Pipe *getPipe(const std::string &);
	virtual Program *extract();
	virtual Program *extract(ParsingScope *);
};

#endif /* !PARSED_HH */

