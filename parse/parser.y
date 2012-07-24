%{
int yylex(void);
extern "C" int yywrap() { return 1; }
void yyerror(const char *);

%}

%union {
	char *str;
}

%token TRETURN TEACH
%token TLBRACKET TRBRACKET
%token TLARROW TRARROW TPERIOD

%token TIDENT

%%

program: statements return

statements:
	| statements statement

statement: pipe | each

pipe: TPERIOD

each: TEACH TPERIOD
	 
return: TRETURN TIDENT TPERIOD

%%

