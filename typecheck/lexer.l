%{

#include "parser_header.hh"
#include "parser.hh"

int yylex();
void yyerror(const char *);
%}

%%

.			yyerror("Bad token");

%%

