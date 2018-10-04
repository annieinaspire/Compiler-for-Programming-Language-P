%{
#include <stdio.h>
#include <stdlib.h>

extern int linenum;             /* declared in lex.l */
extern FILE *yyin;              /* declared by lex */
extern char *yytext;            /* declared by lex */
extern char buf[256];           /* declared in lex.l */
%}

%token KWarray KWbegin KWboolean KWdef KWdo KWelse KWend KWfalse KWfor KWinteger KWor
%token KWif KWof KWprint KWread KWreal KWstring KWthen KWto KWtrue KWreturn KWvar KWwhile
%token ADD SUB MUL DIV MOD ASSIGN LT LE NE GE GT EQ AND OR NOT
%token COMMA SEMICOLON COLON LEFTS RIGHTS LEFTM RIGHTM
%token ID int_cons real_cons str_cons oct_cons SCIEN

%left OR
%left AND
%left NOT
%left LT LE NE GE GT EQ
%left ADD SUB
%left MUL DIV MOD

%%
					/* Don't forget Comp */
program		: ID SEMICOLON StarVCdec StarFdec Comp KWend ID
		;

Fdec		: ID LEFTS StarFaug RIGHTS COLONtype SEMICOLON Comp KWend ID
		;

Faug		: id_list COLON type
		;

id_list		: StarIDCOMMA ID
		;

VCdec		: KWvar id_list COLON sca_type SEMICOLON | 
		  KWvar id_list COLON KWarray int_cons KWto int_cons KWof type SEMICOLON | 
		  KWvar id_list COLON lit_cons SEMICOLON
		;

type		: KWarray int_cons KWto int_cons KWof type | sca_type | lit_cons
		;

sca_type	: KWinteger | KWreal | KWstring | KWboolean
		;

lit_cons	: int_cons | real_cons | str_cons | bool_cons | SCIEN | oct_cons
		;

bool_cons	: KWtrue | KWfalse
		;

Stat		: Comp | Simp | Cond | WHILE | FOR | RETURN | Expr SEMICOLON /* | Fuc_in */
		;

Comp		: KWbegin StarVCdec StarStat KWend
		;

var_ref		: ID StarLEFTMint_consRIGHTM
		;

Simp		: var_ref ASSIGN Expr SEMICOLON | 
		  /*KWprint var_ref SEMICOLON | */  
		  KWprint Expr SEMICOLON | 
		  KWread var_ref SEMICOLON
		;

Func_in		: ID LEFTS StarStarExprCOMMA RIGHTS
		;

Cond	 	: KWif bool_Expr KWthen StarStat KWelse StarStat KWend KWif |
		  KWif bool_Expr KWthen StarStat KWend KWif
		;

WHILE		: KWwhile bool_Expr KWdo StarStat KWend KWdo
		;

FOR		: KWfor ID ASSIGN int_cons KWto int_cons KWdo StarStat KWend KWdo
		;

RETURN		: KWreturn Expr SEMICOLON
		;

Component	: Func_in | lit_cons | var_ref 
		;

Expr		: SUB Expr %prec MUL | Expr MUL Expr | Expr DIV Expr | Expr MOD Expr | 			  Expr ADD Expr | Expr SUB Expr | Expr LT Expr | Expr LE Expr | 
		  Expr NE Expr | Expr GE Expr | Expr GT Expr | Expr EQ Expr | NOT Expr | 			  Expr AND Expr | Expr OR Expr | LEFTS Expr RIGHTS |Component
		;

bool_Expr	: Expr LT Expr | Expr LE Expr | Expr NE Expr | Expr GE Expr | Expr GT Expr 			| Expr EQ Expr
		;

StarVCdec	: 
		| StarVCdec VCdec
		;

StarFdec	: 
		| StarFdec Fdec
		;

StarFaug	: 
		| StarFaug Faug
		;

StarIDCOMMA	: 
		| StarIDCOMMA IDCOMMA
		;

StarStat	: 
		| StarStat Stat
		;

COLONtype	: 
		| COLON type
		;

StarLEFTMint_consRIGHTM	: 
			| StarLEFTMint_consRIGHTM LEFTMint_consRIGHTM
			;

StarStarExprCOMMA	: 
			| StarExprCOMMA Expr
			;

StarExprCOMMA		: 
			| StarExprCOMMA ExprCOMMA
			;

IDCOMMA			: ID COMMA
			;

LEFTMint_consRIGHTM	: LEFTM int_cons RIGHTM
			;

ExprCOMMA		: Expr COMMA
			;


%%

int yyerror( char *msg )
{
        fprintf( stderr, "\n|--------------------------------------------------------------------------\n" );
	fprintf( stderr, "| Error found in Line #%d: %s\n", linenum, buf );
	fprintf( stderr, "|\n" );
	fprintf( stderr, "| Unmatched token: %s\n", yytext );
        fprintf( stderr, "|--------------------------------------------------------------------------\n" );
        exit(-1);
}

int  main( int argc, char **argv )
{
	if( argc != 2 ) {
		fprintf(  stdout,  "Usage:  ./parser  [filename]\n"  );
		exit(0);
	}

	FILE *fp = fopen( argv[1], "r" );
	
	if( fp == NULL )  {
		fprintf( stdout, "Open  file  error\n" );
		exit(-1);
	}
	
	yyin = fp;
	yyparse();

	fprintf( stdout, "\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	fprintf( stdout, "|  There is no syntactic error!  |\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	exit(0);
}

