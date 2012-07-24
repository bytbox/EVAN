%{
int yylex(void);
extern "C" int yywrap() { return 1; }
void yyerror(const char *);

%}

%union {
	char *str;
}

%token TRETURN TEACH
%token TLBRACKET TRBRACKET TLPAREN TRPAREN
%token TLARROW TRARROW TPERIOD

%token <str> TIDENT

%type <str> ident

%%

program: statements return

statements:
	| statements statement

statement: pipe | each

ident: TIDENT { $$ = $1; }

pipe: ident TLARROW ident TPERIOD

each: TEACH TLBRACKET TRBRACKET TPERIOD
	 
return: TRETURN ident TPERIOD

%%

