#include "parse.hh"

#include "parser.hh"

#include <iostream>
#include <string>
#include <sstream>
using namespace std;

extern FILE *yyin;
extern int yyparse();
extern "C" void yyerror(const char *s);

Program *parseProgram(FILE *fin) {
	yyin = fin;
	yyparse();
	return NULL;
}

void yyerror(const char *s) {
	throw s; // TODO
}

