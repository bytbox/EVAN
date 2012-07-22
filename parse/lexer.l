%{
#include "parser.hh"

extern "C" int yylex();
extern "C" void yyerror(const char *);
%}

%%

return		return TRETURN;
\.		return TPERIOD;
.		yyerror("Bad token");

%%

