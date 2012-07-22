%{
extern "C" {
	int yylex(void);
	int yywrap() { return 1; }
	void yyerror(const char *);
}

%}

%token TRETURN TPERIOD

%%

program: statements

statements:
	| statement statements

statement: TRETURN TPERIOD

%%

