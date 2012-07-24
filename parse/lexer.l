%{
#include <cstring>

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
\<-			return TLARROW;
-\>			return TRARROW;
\{			return TLBRACKET;
\}			return TRBRACKET;
\(			return TLPAREN;
\)			return TRPAREN;

{ALPHA}{ALPHANUM}*	yylval.str = strdup(yytext); return TIDENT;
\[[^\]]+\]		yylval.str = strdup(yytext); return TIDENT;

#.*			/* ignored */
\n			/* ignored */
[ \t]+			/* ignored */

.			yyerror("Bad token");

%%

