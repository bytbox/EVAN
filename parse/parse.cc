#include "parse.hh"
#include "parsed.hh"
#include "parser.hh"
#include "parser_header.hh"

#include <iostream>
#include <string>
#include <sstream>
using namespace std;

extern FILE *yyin;
extern int yyparse();
void yyerror(const char *s);

Program *parseProgram(FILE *fin) {
	yyin = fin;
	yyparse();
	return parsed_program->extract();
}

void yyerror(const char *s) {
	throw string(s); // TODO
}

