%{
#include "parse.hh"

extern void yyerror(const char *e);
extern "C" int yywrap();
%}

%%
.		yyerror("Illegal token");
%%

#ifndef yywrap
int yywrap() { return 1; }
#endif

