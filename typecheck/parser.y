%{
#include "parser_header.hh"

int yylex(void);
extern "C" int yywrap() { return 1; }
void yyerror(const char *);
%}

%%

start:

%%

