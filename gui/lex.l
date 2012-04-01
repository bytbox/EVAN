%{
#include "parse.hh"

extern void yyerror(const char *e);
extern "C" int yywrap();
%}

%%
return		return RETURN;
each		return EACH;
\.		return DOT;
.		yyerror("Illegal token");
%%

#ifndef yywrap
int yywrap() { return 1; }
#endif

