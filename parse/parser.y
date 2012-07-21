%{
extern "C" {
	int yyparse(void);
	int yylex(void);
	int yywrap() { return 1; }
}

extern void yyerror(const char *);

%}

%%

program:

