%{
#define YYSTYPE char *

#include "parser.hh"

extern "C" int yylex();
extern "C" void yyerror(const char *);
%}

ALPHA			[a-zA-Z_]
ALPHANUM		[a-zA-Z_0-9]

%%

return			return TRETURN;
each			return TEACH;
\.			return TPERIOD;
[a-zA-Z_]		yylval = strdup(yytext); return TIDENT;
.			yyerror("Bad token");

%%

