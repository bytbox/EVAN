%{
#include <string>

#define YYSTYPE std::string

#include "parser.hh"

int yylex();
void yyerror(const char *);
%}

ALPHA			[a-zA-Z_]
ALPHANUM		[a-zA-Z_0-9]

%%

return			return TRETURN;
each			return TEACH;
\.			return TPERIOD;
{ALPHA}{ALPHANUM}*	yylval = std::string(yytext); return TIDENT;
\n			/* ignored */
[ \t]+			/* ignored */
.			yyerror("Bad token");

%%

