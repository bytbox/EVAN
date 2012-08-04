%{
#include "program.hh"
#include "parsed.hh"

#include "parser_header.hh"

int yylex(void);
extern "C" int yywrap() { return 1; }
void yyerror(const char *);

ParsedProgram *parsed_program;

%}

%union {
	int ival;
	double num;
	std::string *str;
	std::vector <std::string *> *sVec;
	ParsedParam *param;
	std::vector <ParsedParam *> *pVec;
	std::pair <std::string *, ParsedBlock *> *block;
	std::pair <std::string *, ParsedEach *> *each;
	std::pair <std::string *, ParsedPipe *> *pipe;
	std::map <std::string, ParsedPipe *> *pipes;
	ParsedProgram *program;
}

%token TRETURN TEACH
%token TLBRACKET TRBRACKET TLPAREN TRPAREN
%token TPERIOD TCOMMA
%token TLARROW TRARROW TSPLIT TJOIN

%token <str> TIDENT
%token <ival> TINT
%token <num> TNUM

%type <str> ident arg return
%type <sVec> args
%type <param> param
%type <pVec> params param_list
%type <block> block
%type <each> each
%type <pipe> pipe
%type <pipes> statements
%type <program> program

%%

start: program
	{
		parsed_program = $1;
	}

program: statements return
	{
		$$ = new ParsedProgram($1, $2);
	}

statements:
	{
		$$ = new std::map<std::string, ParsedPipe *>;
	}
	| statements pipe {
		auto k = ($2)->first;
		(*($$))[std::string(*k)] = ($2)->second;
		delete k;
	}

pipe:
	block {
		auto p = new ParsedPipe(($1)->second);
		$$ = new std::pair<std::string *, ParsedPipe *>(($1)->first, p);
	}
	| each {
		auto p = new ParsedPipe(($1)->second);
		$$ = new std::pair<std::string *, ParsedPipe *>(($1)->first, p);
	}

block: ident TLARROW ident params args TPERIOD
	{
		auto b = new ParsedBlock($3, $4, $5);
		$$ = new std::pair<std::string *, ParsedBlock *>($1, b);
	}

params:
	{
		$$ = new std::vector<ParsedParam *>{};
	}
	| TLPAREN param_list TRPAREN {
		$$ = $2;
	}

param_list:
	param {
		$$ = new std::vector<ParsedParam *>{$1};
	}
	| param_list TCOMMA param {
		auto v = $1;
		v->push_back($3);
		$$ = v;
	}

param:
	TINT { $$ = new ParsedParam($1); }
	| TNUM { $$ = new ParsedParam($1); }

args:
	{
		$$ = new std::vector<std::string *>{};
	}
	| args TCOMMA arg {
		auto v = $1;
		v->push_back($3);
		$$ = v;
	}

arg:
	TIDENT { $$ = $1; }

each: TEACH ident TSPLIT ident TLBRACKET statements TRBRACKET ident TJOIN ident TPERIOD
	{
		auto b = new ParsedEach($4, $2, $6, $10);
		$$ = new std::pair<std::string *, ParsedEach *>($8, b);
	}
	 
return: TRETURN ident TPERIOD { $$ = $2; }

ident: TIDENT { $$ = $1; }

%%

