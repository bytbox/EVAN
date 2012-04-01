%{
#include <stdio.h>

void yyerror(const char *e) {
	fprintf(stderr, "parse error: %s\n", e);
}

int yywrap() {
	return 1;
}

extern int yylex();
%}

%token LARROW
%token RETURN
%token DOT

%%

program: 
       stmtlist RETURN DOT {} ;

stmtlist: {} ;

