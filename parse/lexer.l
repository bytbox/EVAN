%{
#include "program.hh"
#include "util.hh"

#include <string>
#include <vector>

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
,			return TCOMMA;
\<-			return TLARROW;
-\>			return TRARROW;
-\<			return TJOIN;
\>-			return TSPLIT;
\{			return TLBRACKET;
\}			return TRBRACKET;
\(			return TLPAREN;
\)			return TRPAREN;

[0-9]+\.[0-9]*		yylval.num = ofString<double>(yytext); return TNUM;
[0-9]+			yylval.ival = ofString<int>(yytext); return TINT;

{ALPHA}{ALPHANUM}*	yylval.str = new std::string(yytext); return TIDENT;
\[[^\]]+\]		yylval.str = new std::string(strndup(yytext+1, yyleng-2)); return TIDENT;

#.*			/* ignored */
\n			/* ignored */
[ \t]+			/* ignored */

.			yyerror("Bad token");

%%

