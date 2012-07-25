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
%token TLARROW TRARROW TSPLIT TJOIN TPERIOD

%token <str> TIDENT

%type <str> ident

%%

program: statements return

statements:
	| statements statement

statement: pipe | each

pipe: ident TLARROW ident params args TPERIOD

params:

args:

each: TEACH ident TSPLIT ident TLBRACKET statements TRBRACKET ident TJOIN ident TPERIOD
	 
return: TRETURN ident TPERIOD

ident: TIDENT { $$ = $1; }

%%

