%{
#include "program.hh"

#include <string>
#include <vector>

int yylex(void);
extern "C" int yywrap() { return 1; }
void yyerror(const char *);

%}

%union {
	int ival;
	double num;
	std::string *str;
	std::vector <std::string *> *sVec;
	Param *par;
	std::vector <Param *> *pVec;
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
%type <par> param
%type <pVec> params param_list

%%

program: statements return

statements:
	| statements statement

statement: pipe | each

pipe: ident TLARROW ident params args TPERIOD

params:
	{
		$$ = new std::vector<Param *>{};
	}
	| TLPAREN param_list TRPAREN {
		$$ = $2;
	}

param_list:
	{
		$$ = new std::vector<Param *>{};
	}
	| param_list TCOMMA param {
		auto v = $1;
		v->push_back($3);
		$$ = v;
	}

param:
	TINT { $$ = new Param($1); }
	| TNUM { $$ = new Param($1); }

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

each: TEACH ident TSPLIT ident TLBRACKET statements TRBRACKET ident TJOIN ident TPERIOD
	 
return: TRETURN ident TPERIOD

ident: TIDENT { $$ = $1; }

%%

