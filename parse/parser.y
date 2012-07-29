%{
#include "program.hh"
#include "parsed.hh"

#include "parser_header.hh"

int yylex(void);
extern "C" int yywrap() { return 1; }
void yyerror(const char *);

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
}

%token TRETURN TEACH
%token TLBRACKET TRBRACKET TLPAREN TRPAREN
%token TPERIOD TCOMMA
%token TLARROW TRARROW TSPLIT TJOIN

%token <str> TIDENT
%token <ival> TINT
%token <num> TNUM

%type <str> ident arg
%type <sVec> args
%type <param> param
%type <pVec> params param_list
%type <block> block
%type <each> each

%%

program: statements return

statements:
	| statements statement

statement: pipe
	 
pipe: block | each

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
	{
		$$ = new std::vector<ParsedParam *>{};
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
		$$ = NULL;
	}
	 
return: TRETURN ident TPERIOD

ident: TIDENT { $$ = $1; }

%%

