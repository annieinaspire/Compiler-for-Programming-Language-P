%{
/**
 * Introduction to Compiler Design by Prof. Yi Ping You
 * Project 3 YACC sample
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "header.h"
#include "symtab.h"
#include "semcheck.h"

//#include "test.h"

int yydebug;

extern int linenum;		/* declared in lex.l */
extern FILE *yyin;		/* declared by lex */
extern FILE *yyout;
extern char *yytext;		/* declared by lex */
extern char buf[256];		/* declared in lex.l */
extern int yylex(void);
int yyerror(char* );

int scope = 0;
int next_num=0;
int label_num=0,if_label_num,while_label_num;
int in_func_decl=0;
int isRead=0;

int Opt_D = 1;			/* symbol table dump option */
char fileName[256];
char output_fileName[256];

struct SymTable *symbolTable;	// main symbol table
struct Table *localTable;

__BOOLEAN paramError;			// indicate is parameter have any error?

struct PType *funcReturn;		// record function's return type, used at 'return statement' production rule

%}

%union {
	int intVal;
	float realVal;
	//__BOOLEAN booleanVal;
	char *lexeme;
	struct idNode_sem *id;
	//SEMTYPE type;
	struct ConstAttr *constVal;
	struct PType *ptype;
	struct param_sem *par;
	struct expr_sem *exprs;
	/*struct var_ref_sem *varRef; */
	struct expr_sem_node *exprNode;
};

/* tokens */
%token ARRAY BEG BOOLEAN DEF DO ELSE END FALSE FOR INTEGER IF OF PRINT READ REAL RETURN STRING THEN TO TRUE VAR WHILE
%token OP_ADD OP_SUB OP_MUL OP_DIV OP_MOD OP_ASSIGN OP_EQ OP_NE OP_GT OP_LT OP_GE OP_LE OP_AND OP_OR OP_NOT
%token MK_COMMA MK_COLON MK_SEMICOLON MK_LPAREN MK_RPAREN MK_LB MK_RB

%token <lexeme>ID
%token <intVal>INT_CONST 
%token <realVal>FLOAT_CONST
%token <realVal>SCIENTIFIC
%token <lexeme>STR_CONST

%type<id> id_list
%type<constVal> literal_const
%type<ptype> type scalar_type array_type opt_type
%type<par> param param_list opt_param_list
%type<exprs> var_ref boolean_expr boolean_term boolean_factor relop_expr expr term factor boolean_expr_list opt_boolean_expr_list
%type<intVal> dim mul_op add_op rel_op array_index loop_param

/* start symbol */
%start program
%%

program			: ID
			{
			  struct PType *pType = createPType( VOID_t );
			  struct SymNode *newNode = createProgramNode( $1, scope, pType );
			  insertTab( symbolTable, newNode );

			  if( strcmp(fileName,$1) ) {
				fprintf( stdout, "########## Error at Line#%d: program beginning ID inconsist with file name ########## \n", linenum );
			  }
			
	   localTable=(struct Table *)malloc(sizeof(struct Table));			  
			  initialize_table(localTable);
			  fprintf(yyout,".class public %s\n",$1);
			  fprintf(yyout,".super java/lang/Object\n");
			  fprintf(yyout,".field public static _sc Ljava/util/Scanner;\n");
			}
			  MK_SEMICOLON 
			  program_body
			  END ID
			{
			  if( strcmp($1, $6) ) { fprintf( stdout, "########## Error at Line #%d: %s", linenum,"Program end ID inconsist with the beginning ID ########## \n"); }
			  if( strcmp(fileName,$6) ) {
				 fprintf( stdout, "########## Error at Line#%d: program end ID inconsist with file name ########## \n", linenum );
			  }
			  // dump symbol table
			  if( Opt_D == 1 )
				printSymTable( symbolTable, scope );

			  delete_all_local(localTable);
			  next_num=0;
			}
			;

program_body		: opt_decl_list opt_func_decl_list compound_stmt
			{
			  fprintf(yyout,"    return\n");
			  fprintf(yyout,".end method\n");
			}
			;

opt_decl_list		: decl_list
			| /* epsilon */
			;

decl_list		: decl_list decl
			| decl
			;

decl			: VAR id_list MK_COLON scalar_type MK_SEMICOLON       /* scalar type declaration */
			{
			  // insert into symbol table
			  struct idNode_sem *ptr;
			  struct SymNode *newNode;
			  for( ptr=$2 ; ptr!=0 ; ptr=(ptr->next) ) {
			  	if( verifyRedeclaration( symbolTable, ptr->value, scope ) ==__FALSE ) { }
				else {
					newNode = createVarNode( ptr->value, scope, $4 );
		if(scope==0){
			if($4->type==1)		
			fprintf(yyout,".field public static %s %s\n",ptr->value,"I");
			else if($4->type==2)
			fprintf(yyout,".field public static %s %s\n",ptr->value,"Z");
			else if($4->type==4)
			fprintf(yyout,".field public static %s %s\n",ptr->value,"F");
		}
		else{
			  push_one_local(localTable,ptr->value,next_num);
			  next_num++;
			  print_local_table(localTable);
		}
					insertTab( symbolTable, newNode );
				}
			  }
			  
			  deleteIdList( $2 );
			}
			| VAR id_list MK_COLON array_type MK_SEMICOLON        /* array type declaration */
			{
			  verifyArrayType( $2, $4 );
			  // insert into symbol table
			  struct idNode_sem *ptr;
			  struct SymNode *newNode;
			  for( ptr=$2 ; ptr!=0 ; ptr=(ptr->next) ) {
			  	if( $4->isError == __TRUE ) { }
				else if( verifyRedeclaration( symbolTable, ptr->value, scope ) ==__FALSE ) { }
				else {
					newNode = createVarNode( ptr->value, scope, $4 );
					insertTab( symbolTable, newNode );
				}
			  }
			  
			  deleteIdList( $2 );
			}
			| VAR id_list MK_COLON literal_const MK_SEMICOLON     /* const declaration */
			{
			  struct PType *pType = createPType( $4->category );
			  // insert constants into symbol table
			  struct idNode_sem *ptr;
			  struct SymNode *newNode;
			  for( ptr=$2 ; ptr!=0 ; ptr=(ptr->next) ) {
			  	if( verifyRedeclaration( symbolTable, ptr->value, scope ) ==__FALSE ) { }
				else {
					newNode = createConstNode( ptr->value, scope, pType, $4 );
					insertTab( symbolTable, newNode );
				}
			  }
			  
			  deleteIdList( $2 );
			}
			;

literal_const		: INT_CONST
			{
			  int tmp = $1;
			  $$ = createConstAttr( INTEGER_t, &tmp );
			}
			| OP_SUB INT_CONST
			{
			  int tmp = -$2;
			  $$ = createConstAttr( INTEGER_t, &tmp );
			}
			| FLOAT_CONST
			{
			  float tmp = $1;
			  $$ = createConstAttr( REAL_t, &tmp );
			}
			| OP_SUB FLOAT_CONST
			{
			  float tmp = -$2;
			  $$ = createConstAttr( REAL_t, &tmp );
			}
			| SCIENTIFIC 
			{
			  float tmp = $1;
			  $$ = createConstAttr( REAL_t, &tmp );
			}
			| OP_SUB SCIENTIFIC
			{
			  float tmp = -$2;
			  $$ = createConstAttr( REAL_t, &tmp );
			}
			| STR_CONST
			{
			  $$ = createConstAttr( STRING_t, $1 );
			}
			| TRUE
			{
			  __BOOLEAN tmp = __TRUE;
			  $$ = createConstAttr( BOOLEAN_t, &tmp );
			}
			| FALSE
			{
			  __BOOLEAN tmp = __FALSE;
			  $$ = createConstAttr( BOOLEAN_t, &tmp );
			}
			;

opt_func_decl_list	: func_decl_list
			| /* epsilon */
			;

func_decl_list		: func_decl_list func_decl
			| func_decl
			;

func_decl		: ID MK_LPAREN opt_param_list
			{
			  in_func_decl=1;
			  // check and insert parameters into symbol table
			  paramError = insertParamIntoSymTable( symbolTable, $3, scope+1 );
			}
			  MK_RPAREN opt_type 
			{
			  // check and insert function into symbol table
			  if( paramError == __TRUE ) {
			  	printf("--- param(s) with several fault!! ---\n");
			  } else {
				insertFuncIntoSymTable( symbolTable, $1, $3, $6, scope );
			  }
			  funcReturn = $6;

			  struct SymNode* temp=lookupSymbol(symbolTable,$1,scope,__TRUE);
			  //printf("WWWWWWW%d\n",temp->type->type);
			  fprintf(yyout,".method public static %s(",$1);
			  		int i;
                                        struct PTypeList *pTypePtr;
                                        for( i=0, pTypePtr=(temp->attribute->formalParam->params) ; i<(temp->attribute->formalParam->paramNum) ; i++, pTypePtr=(pTypePtr->next) ) {
                                                //printType( pTypePtr->value, 0 );
						//printf("(%d)",pTypePtr->value->type);

				if(pTypePtr->value->type==1)fprintf(yyout,"I");
				else if(pTypePtr->value->type==2)fprintf(yyout,"Z");
				else if(pTypePtr->value->type==3)fprintf(yyout,"C");
				else if(pTypePtr->value->type==4)fprintf(yyout,"F");
                                                //printf(", ");
					}
					fprintf(yyout,")");

				if($6->type==1)fprintf(yyout,"I\n");
				else if($6->type==2)fprintf(yyout,"Z\n");
				else if($6->type==3)fprintf(yyout,"C\n");
				else if($6->type==4)fprintf(yyout,"F\n");

				fprintf(yyout,".limit stack 20\n");
				fprintf(yyout,".limit locals 20\n");


			}
			  MK_SEMICOLON
			  compound_stmt
			  END ID
			{
			  if( strcmp($1,$11) ) {
				fprintf( stdout, "########## Error at Line #%d: the end of the functionName mismatch ########## \n", linenum );
			  }
			  funcReturn = 0;

			  fprintf(yyout,"    return\n");
			  fprintf(yyout,".end method\n");

			  in_func_decl=0;

			  delete_all_local(localTable);
			  next_num=0;
			}
			;

opt_param_list		: param_list { $$ = $1; }
			| /* epsilon */ { $$ = 0; }
			;

param_list		: param_list MK_SEMICOLON param
			{
			  param_sem_addParam( $1, $3 );
			  $$ = $1;
			}
			| param { $$ = $1; }
			;

param			: id_list MK_COLON type 
			{ 
			  struct idNode_sem *ptr;

			  for( ptr=$1 ; ptr!=0 ; ptr=(ptr->next) ) {
				push_one_local(localTable,ptr->value,next_num);
				next_num++;
			  }
			  print_local_table(localTable);
			  $$ = createParam( $1, $3 ); 
			}
			;

id_list			: id_list MK_COMMA ID
			{
			  idlist_addNode( $1, $3 );
			  $$ = $1;
			}
			| ID { $$ = createIdList($1); }
			;

opt_type		: MK_COLON type { $$ = $2; }
			| /* epsilon */ { $$ = createPType( VOID_t ); }
			;

type			: scalar_type { $$ = $1; }
			| array_type { $$ = $1; }
			;

scalar_type		: INTEGER { $$ = createPType( INTEGER_t ); }
			| REAL { $$ = createPType( REAL_t ); }
			| BOOLEAN { $$ = createPType( BOOLEAN_t ); }
			| STRING { $$ = createPType( STRING_t ); }
			;

array_type		: ARRAY array_index TO array_index OF type
			{
				verifyArrayDim( $6, $2, $4 );
				increaseArrayDim( $6, $2, $4 );
				$$ = $6;
			}
			;

array_index		: INT_CONST { $$ = $1; }
			| OP_SUB INT_CONST { $$ = -$2; }
			;

stmt			: compound_stmt
			| simple_stmt
			| cond_stmt
			| while_stmt
			| for_stmt
			| return_stmt
			| proc_call_stmt
			;

compound_stmt		: 
			{ 
			  if(scope==0)next_num=1;
		if(!in_func_decl && scope==0){
		fprintf(yyout,".method public static main([Ljava/lang/String;)V\n");
			  fprintf(yyout,".limit stack 20\n");
			  fprintf(yyout,".limit locals 20\n");
			  fprintf(yyout,"new java/util/Scanner\n");
			  fprintf(yyout,"dup\n");
		fprintf(yyout,"getstatic java/lang/System/in Ljava/io/InputStream;\n");
	fprintf(yyout,"invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V\n");
			  fprintf(yyout,"putstatic %s/_sc Ljava/util/Scanner;\n",fileName);
			  }
			  scope++;
			}
			  BEG
			  opt_decl_list
			  opt_stmt_list
			  END 
			{ 
			  // print contents of current scope
			  if( Opt_D == 1 )
			  	printSymTable( symbolTable, scope );
			  deleteScope( symbolTable, scope );	// leave this scope, delete...
			  scope--; 
			}
			;

opt_stmt_list		: stmt_list
			| /* epsilon */
			;

stmt_list		: stmt_list stmt
			| stmt
			;

NT1			:
			{
	       fprintf(yyout,"    getstatic java/lang/System/out Ljava/io/PrintStream;\n");
			}
			;

simple_stmt		: var_ref OP_ASSIGN boolean_expr MK_SEMICOLON
			{
			  // check if LHS exists
			  __BOOLEAN flagLHS = verifyExistence( symbolTable, $1, scope, __TRUE );
			  // id RHS is not dereferenced, check and deference
			  __BOOLEAN flagRHS = __TRUE;
			  if( $3->isDeref == __FALSE ) {
				flagRHS = verifyExistence( symbolTable, $3, scope, __FALSE );
			  }
			  // if both LHS and RHS are exists, verify their type
			  if( flagLHS==__TRUE && flagRHS==__TRUE )
				verifyAssignmentTypeMatch( $1, $3 );

			  /*if($3->isConst==1){//printf("$$$$$$$$%d\n",$3->value.integerVal);
			  	switch( $3->pType->type ) {
				 case INTEGER_t:
				 fprintf(yyout,"    sipush %d\n",$3->value.integerVal);
					break;
				 case REAL_t:
				 fprintf(yyout,"    ldc %f\n",$3->value.realVal);
					break;
				 case BOOLEAN_t:
				 	if($3->value.booleanVal==0)
				 	fprintf(yyout,"    iconst_0\n");
					else if($3->value.booleanVal==1)
					fprintf(yyout,"    iconst_1\n");
					break;
				 case STRING_t:
				 fprintf(yyout,"    ldc \"%s\"\n",$3->value.stringVal);
					break;
				 default:
					break;
			  	}
			  }*/

			struct Symbol *temp=(struct Symbol *)malloc(sizeof(struct Symbol));
			  temp=search_for_symbol(localTable,$1->varRef->id);
			
			if(temp==NULL){
			  if($1->pType->type==1){
			  fprintf(yyout,"    putstatic %s/%s I\n",fileName,$1->varRef->id);
			  }
			  else if($1->pType->type==2){
			  fprintf(yyout,"    putstatic %s/%s Z\n",fileName,$1->varRef->id);
			  }
			  else if($1->pType->type==4){
			  fprintf(yyout,"    putstatic %s/%s F\n",fileName,$1->varRef->id);
			  }
			}
			else{
			  if($1->pType->type==1){
			  fprintf(yyout,"    istore %d\n",temp->number);
			  }
			  else if($1->pType->type==2){
			  fprintf(yyout,"    istore %d\n",temp->number);
			  }
			  else if($1->pType->type==4){
			  fprintf(yyout,"    fstore %d\n",temp->number);
			  }
			}


			}
			| PRINT NT1 boolean_expr MK_SEMICOLON 
			{ 
			  verifyScalarExpr( $3, "print" ); 

			  if($3->isConst==1){//printf("$$$$$$$$%d\n",$2->value.integerVal);
			  	switch( $3->pType->type ) {
				 case INTEGER_t:
				 //fprintf(yyout,"    sipush %d\n",$3->value.integerVal);
			fprintf(yyout,"    invokevirtual java/io/PrintStream/print(I)V\n");
					break;
				 case REAL_t:
				 //fprintf(yyout,"    ldc %f\n",$3->value.realVal);
			fprintf(yyout,"    invokevirtual java/io/PrintStream/print(F)V\n");
					break;
				 case BOOLEAN_t:
				 	/*if($3->value.booleanVal==0)
				 	fprintf(yyout,"    iconst_0\n");
				 	else if($3->value.booleanVal==1)
				 	fprintf(yyout,"    iconst_1\n");*/
			fprintf(yyout,"    invokevirtual java/io/PrintStream/print(Z)V\n");
					break;
				 case STRING_t:
				 //fprintf(yyout,"    ldc \"%s\"\n",$3->value.stringVal);
       fprintf(yyout,"    invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
					break;
				 default:
					break;
			  	}
			  }
			  else {
				//printf("&&&&&&&&%s\n",$2->varRef->id);
			struct Symbol *temp=(struct Symbol *)malloc(sizeof(struct Symbol));
			  temp=search_for_symbol(localTable,$3->varRef->id);

			  switch( $3->pType->type ) {
				 case INTEGER_t:
				 //fprintf(yyout,"    iload %d\n",temp->number);
			fprintf(yyout,"    invokevirtual java/io/PrintStream/print(I)V\n");
					break;
				 case REAL_t:
				 //fprintf(yyout,"    fload %d\n",temp->number);
			fprintf(yyout,"    invokevirtual java/io/PrintStream/print(F)V\n");
					break;
				 case BOOLEAN_t:
				 //fprintf(yyout,"    iload %d\n",temp->number);
			fprintf(yyout,"    invokevirtual java/io/PrintStream/print(Z)V\n");
					break;
				case STRING_t:
				 //fprintf(yyout,"    iload %d\n",temp->number);
       fprintf(yyout,"    invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
					break;
				 default:
					break;
			  	}
			  }
			}
 			| READ NT4 NT3
			;

NT4			:
			{
				isRead=1;
			}
			;

NT3			: boolean_expr MK_SEMICOLON 
			{ 
			  verifyScalarExpr( $1, "read" ); 
			  fprintf(yyout,"getstatic %s/_sc Ljava/util/Scanner;\n",fileName);

			struct Symbol *temp=(struct Symbol *)malloc(sizeof(struct Symbol));
			  temp=search_for_symbol(localTable,$1->varRef->id);

			if(temp==NULL){
			  if($1->pType->type==1){
			  fprintf(yyout,"invokevirtual java/util/Scanner/nextInt()I\n");
			  fprintf(yyout,"putstatic %s/%s I\n",fileName,$1->varRef->id);
			  }
			  else if($1->pType->type==2){
			 fprintf(yyout,"invokevirtual java/util/Scanner/nextBoolean()Z\n");
			  fprintf(yyout,"putstatic %s/%s Z\n",fileName,$1->varRef->id);
			  }
			  else if($1->pType->type==4){
			  fprintf(yyout,"invokevirtual java/util/Scanner/nextFloat()F\n");
			  fprintf(yyout,"putstatic %s/%s F\n",fileName,$1->varRef->id);
			  }
			}
			else{
			  if($1->pType->type==1){
			  fprintf(yyout,"invokevirtual java/util/Scanner/nextInt()I\n");
			  fprintf(yyout,"istore %d\n",temp->number);
			  }
			  else if($1->pType->type==2){
			 fprintf(yyout,"invokevirtual java/util/Scanner/nextBoolean()Z\n");
			  fprintf(yyout,"istore %d\n",temp->number);
			  }
			  else if($1->pType->type==4){
			  fprintf(yyout,"invokevirtual java/util/Scanner/nextFloat()F\n");
			  fprintf(yyout,"fstore %d\n",temp->number);
			  }
			}
		
			isRead=0;

			}
			;

proc_call_stmt		: ID MK_LPAREN opt_boolean_expr_list MK_RPAREN MK_SEMICOLON
			{
			  verifyFuncInvoke( $1, $3, symbolTable, scope );
			}
			;

cond_stmt		: IF 
			{
				if_label_num=label_num;
				fprintf(yyout,"Ltest_%d:\n",label_num);
			} 
			  condition THEN
			  opt_stmt_list
			  NT2
			;

NT2			: END IF
			{
				fprintf(yyout,"Lelse_%d:\n",if_label_num);
				//label_num++;
			}
			| ELSE NT0
			  opt_stmt_list
			  END IF
			{
				fprintf(yyout,"Lexit_%d:\n",if_label_num);
				//label_num++;
			}
			;


NT0			: 
			{
			  fprintf(yyout,"    goto Lexit_%d\n",if_label_num);
			  fprintf(yyout,"Lelse_%d:\n",if_label_num);
			}
			;

condition		: boolean_expr 
			{ 
			  verifyBooleanExpr( $1, "if" ); 
			  fprintf(yyout,"    ifeq Lelse_%d\n",if_label_num);
			} 
			;

while_stmt		: WHILE
			{
				while_label_num=label_num;
				fprintf(yyout,"Ltest_%d:\n",while_label_num);
			} 
			  condition_while DO
			  opt_stmt_list
			  END DO
			{
				fprintf(yyout,"    goto Ltest_%d\n",while_label_num);
				fprintf(yyout,"Lelse_%d:\n",while_label_num);
				fprintf(yyout,"Lexit_%d:\n",while_label_num);
				//label_num++;
			}
			;

condition_while		: boolean_expr 
			{ 
			verifyBooleanExpr( $1, "while" ); 
		fprintf(yyout,"    ifeq Lelse_%d\n",while_label_num);			
			} 
			;

for_stmt		: FOR ID 
			{ 
			  insertLoopVarIntoTable( symbolTable, $2 );
			  push_one_local(localTable,$2,next_num);
			  next_num++;
			  print_local_table(localTable);
			}
			  OP_ASSIGN loop_param TO loop_param
			{
			  verifyLoopParam( $5, $7 );
			  
				struct Symbol *temp=search_for_symbol(localTable,$2);
				fprintf(yyout,"    sipush %d\n",$5);
				fprintf(yyout,"    istore %d\n",temp->number);
				fprintf(yyout,"Lbegin_%d:\n",label_num);
				fprintf(yyout,"    iload %d\n",temp->number);
				fprintf(yyout,"    sipush %d\n",$7+1);
				fprintf(yyout,"    isub\n");
				fprintf(yyout,"    iflt Ltrue_%d\n",label_num);
				fprintf(yyout,"    iconst_0\n");
				fprintf(yyout,"    goto Lfalse_%d\n",label_num);
				fprintf(yyout,"Ltrue_%d:\n",label_num);
				fprintf(yyout,"    iconst_1\n");
				fprintf(yyout,"Lfalse_%d:\n",label_num);
				fprintf(yyout,"    ifeq Lexit_%d\n",label_num);
			}
			  DO
			  opt_stmt_list
			{
				struct Symbol *temp=search_for_symbol(localTable,$2);
				fprintf(yyout,"    iload %d\n",temp->number);
				fprintf(yyout,"    sipush %d\n",1);
				fprintf(yyout,"    iadd\n");
				fprintf(yyout,"    istore %d\n",temp->number);
				fprintf(yyout,"    goto Lbegin_%d\n",label_num);
				fprintf(yyout,"Lexit_%d:\n",label_num);
			}
			  END DO
			{
			  popLoopVar( symbolTable );
			  label_num++;
			}
			;

loop_param		: INT_CONST { $$ = $1; }
			| OP_SUB INT_CONST { $$ = -$2; }
			;

return_stmt		: RETURN boolean_expr MK_SEMICOLON
			{
			  verifyReturnStatement( $2, funcReturn );
			  /*if($2->isConst==1){//printf("$$$$$$$$%d\n",$2->value.integerVal);
			  	switch( $2->pType->type ) {
				 case INTEGER_t:
				 fprintf(yyout,"    sipush %d\n",$2->value.integerVal);
					break;
				 case REAL_t:
				 fprintf(yyout,"    ldc %f\n",$2->value.realVal);
					break;
				 case BOOLEAN_t:
				 	if($2->value.booleanVal==0)
				 	fprintf(yyout,"    iconst_0\n");
					else if($2->value.booleanVal==1)
					fprintf(yyout,"    iconst_1\n");
					break;
				 case STRING_t:
				 fprintf(yyout,"    ldc \"%s\"\n",$2->value.stringVal);
					break;
				 default:
					break;
			  	}
			  }*/
			  /*else {
				//printf("*******%s\n",$2->varRef->id);
			struct Symbol *temp=(struct Symbol *)malloc(sizeof(struct Symbol));
			  temp=search_for_symbol(localTable,$2->varRef->id);
			  if($2->pType->type==1||$2->pType->type==2)
			  fprintf(yyout,"    iload %d\n",temp->number);
			  else if($2->pType->type==4)
			  fprintf(yyout,"    fload %d\n",temp->number);
			  }*/
			
			  fprintf(yyout,"    ireturn\n");
			}
			;

opt_boolean_expr_list	: boolean_expr_list { $$ = $1; }
			| /* epsilon */ { $$ = 0; }	// null
			;

boolean_expr_list	: boolean_expr_list MK_COMMA boolean_expr
			{
			  struct expr_sem *exprPtr;
			  for( exprPtr=$1 ; (exprPtr->next)!=0 ; exprPtr=(exprPtr->next) );
			  exprPtr->next = $3;
			  $$ = $1;
			}
			| boolean_expr
			{
			  $$ = $1;
			}
			;

boolean_expr		: boolean_expr OP_OR boolean_term
			{
			  verifyAndOrOp( $1, OR_t, $3 );
			  $$ = $1;

			  if($1->pType->type==2 && $3->pType->type==2)
			  fprintf(yyout,"    ior\n");
			}
			| boolean_term { $$ = $1; }
			;

boolean_term		: boolean_term OP_AND boolean_factor
			{
			  verifyAndOrOp( $1, AND_t, $3 );
			  $$ = $1;
			  
			  if($1->pType->type==2 && $3->pType->type==2)
			  fprintf(yyout,"    iand\n");
			}
			| boolean_factor { $$ = $1; }
			;

boolean_factor		: OP_NOT boolean_factor 
			{
			  verifyUnaryNOT( $2 );
			  $$ = $2;

			  if($2->pType->type==2)
			  fprintf(yyout,"    inot\n");
			}
			| relop_expr { $$ = $1; }
			;

relop_expr		: expr rel_op expr
			{
			  verifyRelOp( $1, $2, $3 );
			  $$ = $1;

				if($1->pType->type==1 || $3->pType->type==1)
				fprintf(yyout,"    isub\n");
				else if($1->pType->type==4 && $3->pType->type==4)
				fprintf(yyout,"    fcmpl\n");

				switch( $2 ) {
				 case LT_t:
				 fprintf(yyout,"    iflt Ltrue_%d\n",label_num);
					break;
				 case LE_t:
				 fprintf(yyout,"    ifle Ltrue_%d\n",label_num);
					break;
				 case EQ_t:
				 fprintf(yyout,"    ifeq Ltrue_%d\n",label_num);
					break;
				 case GE_t:
				 fprintf(yyout,"    ifge Ltrue_%d\n",label_num);
					break;
				 case GT_t:
				 fprintf(yyout,"    ifgt Ltrue_%d\n",label_num);
					break;
				 case NE_t:
				 fprintf(yyout,"    ifne Ltrue_%d\n",label_num);
					break;
				 default:
					/* FIXME */
					break;
				}

				fprintf(yyout,"    iconst_0\n");
				fprintf(yyout,"    goto Lfalse_%d\n",label_num);
				fprintf(yyout,"Ltrue_%d:\n",label_num);
				fprintf(yyout,"    iconst_1\n");
				fprintf(yyout,"Lfalse_%d:\n",label_num);
				label_num++;
				//fprintf(yyout,"    ifeq Lelse_%d\n",label_num);
			}
			| expr 
			{ 
				$$ = $1; 
			}
			;

rel_op			: OP_LT { $$ = LT_t; }
			| OP_LE { $$ = LE_t; }
			| OP_EQ { $$ = EQ_t; }
			| OP_GE { $$ = GE_t; }
			| OP_GT { $$ = GT_t; }
			| OP_NE { $$ = NE_t; }
			;

expr			: expr add_op term
			{
			  verifyArithmeticOp( $1, $2, $3 );
			  $$ = $1;
			/*if($1->isConst==1){
				switch( $1->pType->type ) {
				 case INTEGER_t:
				 fprintf(yyout,"    sipush %d\n",$1->value.integerVal);
					break;
				 case REAL_t:
				 fprintf(yyout,"    ldc %f\n",$1->value.realVal);
					break;
				 case BOOLEAN_t:
				 	if($1->value.booleanVal==0)
				 	fprintf(yyout,"    iconst_0\n");
					else if($1->value.booleanVal==1)
					fprintf(yyout,"    iconst_1\n");
					break;
				 case STRING_t:
				 fprintf(yyout,"    ldc \"%s\"\n",$1->value.stringVal);
					break;
				 default:
					break;
				}
			}*/
			/*else{
			struct Symbol *t1=(struct Symbol *)malloc(sizeof(struct Symbol));
			  t1=search_for_symbol(localTable,$1->varRef->id);
			  if($1->pType->type==1||$1->pType->type==2)
			  fprintf(yyout,"    iload %d\n",t1->number);
			  else if($1->pType->type==4)
			  fprintf(yyout,"    fload %d\n",t1->number);
			}*/
			
			/*if($3->isConst==1){
				switch( $3->pType->type ) {
				 case INTEGER_t:
				 fprintf(yyout,"    sipush %d\n",$3->value.integerVal);
					break;
				 case REAL_t:
				 fprintf(yyout,"    ldc %f\n",$3->value.realVal);
					break;
				 case BOOLEAN_t:
				 	if($3->value.booleanVal==0)
				 	fprintf(yyout,"    iconst_0\n");
					else if($3->value.booleanVal==1)
					fprintf(yyout,"    iconst_1\n");
					break;
				 case STRING_t:
				 fprintf(yyout,"    ldc \"%s\"\n",$3->value.stringVal);
					break;
				 default:
					break;
				}
			}*/
			/*else{
			struct Symbol *t2=(struct Symbol *)malloc(sizeof(struct Symbol));
			  t2=search_for_symbol(localTable,$3->varRef->id);
			  if($3->pType->type==1||$3->pType->type==2)
			  fprintf(yyout,"    iload %d\n",t2->number);
			  else if($3->pType->type==4)
			  fprintf(yyout,"    fload %d\n",t2->number);
			}*/
				
				switch( $2 ) {
				 case ADD_t:
					if($1->pType->type==1 && $3->pType->type==1)
				 	fprintf(yyout,"    iadd\n");
					else if($1->pType->type==4 && $3->pType->type==4)
				 	fprintf(yyout,"    fadd\n");
					break;
				 case SUB_t:
					if($1->pType->type==1 && $3->pType->type==1)
				 	fprintf(yyout,"    isub\n");
					else if($1->pType->type==4 && $3->pType->type==4)
				 	fprintf(yyout,"    fsub\nf2i\n");
					break;
				 default:
					/* FIXME */
					break;
				}
			}
			| term { $$ = $1; }
			;

add_op			: OP_ADD { $$ = ADD_t; }
			| OP_SUB { $$ = SUB_t; }
			;

term			: term mul_op factor
			{
			  if( $2 == MOD_t ) {
				verifyModOp( $1, $3 );
			  }
			  else {
				verifyArithmeticOp( $1, $2, $3 );
			  }
			  $$ = $1;

			
			/*if($1->isConst==1){
				switch( $1->pType->type ) {
				 case INTEGER_t:
				 fprintf(yyout,"    sipush %d\n",$1->value.integerVal);
					break;
				 case REAL_t:
				 fprintf(yyout,"    ldc %f\n",$1->value.realVal);
					break;
				 case BOOLEAN_t:
				 	if($1->value.booleanVal==0)
				 	fprintf(yyout,"    iconst_0\n");
					else if($1->value.booleanVal==1)
					fprintf(yyout,"    iconst_1\n");
					break;
				 case STRING_t:
				 fprintf(yyout,"    ldc \"%s\"\n",$1->value.stringVal);
					break;
				 default:
					break;
				}
			}*/
			/*else{
			struct Symbol *t1=(struct Symbol *)malloc(sizeof(struct Symbol));
			  t1=search_for_symbol(localTable,$1->varRef->id);
			  if($1->pType->type==1||$1->pType->type==2)
			  fprintf(yyout,"    iload %d\n",t1->number);
			  else if($1->pType->type==4)
			  fprintf(yyout,"    fload %d\n",t1->number);
			}*/

			/*if($3->isConst==1){
				switch( $3->pType->type ) {
				 case INTEGER_t:
				 fprintf(yyout,"    sipush %d\n",$3->value.integerVal);
					break;
				 case REAL_t:
				 fprintf(yyout,"    ldc %f\n",$3->value.realVal);
					break;
				 case BOOLEAN_t:
				 	if($1->value.booleanVal==0)
				 	fprintf(yyout,"    iconst_0\n");
					else if($1->value.booleanVal==1)
					fprintf(yyout,"    iconst_1\n");
					break;
				 case STRING_t:
				 fprintf(yyout,"    ldc \"%s\"\n",$3->value.stringVal);
					break;
				 default:
					break;
				}
			}*/
			/*else{
			struct Symbol *t2=(struct Symbol *)malloc(sizeof(struct Symbol));
			  t2=search_for_symbol(localTable,$3->varRef->id);
			  if($3->pType->type==1||$3->pType->type==2)
			  fprintf(yyout,"    iload %d\n",t2->number);
			  else if($3->pType->type==4)
			  fprintf(yyout,"    fload %d\n",t2->number);
			}*/

				switch( $2 ) {
				 case MUL_t:
					if($1->pType->type==1 && $3->pType->type==1)
				 	fprintf(yyout,"    imul\n");
					else if($1->pType->type==4 && $3->pType->type==4)
				 	fprintf(yyout,"    fmul\n");
					break;
				 case DIV_t:
					if($1->pType->type==1 && $3->pType->type==1)
				 	fprintf(yyout,"    idiv\n");
					else if($1->pType->type==4 && $3->pType->type==4)
				 	fprintf(yyout,"    fdiv\n");
					break;
				 case MOD_t:
					if($1->pType->type==1 && $3->pType->type==1)
				 	fprintf(yyout,"    irem\n");
					break;
				 default:
					/* FIXME */
					break;
				}
			}
			| factor { $$ = $1; }
			;

mul_op			: OP_MUL { $$ = MUL_t; }
			| OP_DIV { $$ = DIV_t; }
			| OP_MOD { $$ = MOD_t; }
			;

factor			: var_ref
			{
			  verifyExistence( symbolTable, $1, scope, __FALSE );
			  $$ = $1;
			  $$->beginningOp = NONE_t;

if(!isRead){
	struct SymNode* t;struct Symbol* tt;
	t=lookupSymbol(symbolTable,$1->varRef->id,scope,__FALSE);
	if(t->scope==0){//fprintf(yyout,"wheretype: %d\n",t->type->type);
		if(t->type->type==1 || t->type->type==2)
		fprintf(yyout,"    getstatic %s/%s I\n",fileName,$1->varRef->id);
		else if(t->type->type==4)
		fprintf(yyout,"    getstatic %s/%s F\n",fileName,$1->varRef->id);
		else if(t->type->type==3)
		fprintf(yyout,"    ldc \"%s\"\n",t->attribute->constVal->value.stringVal);
	}
	else{
		tt=search_for_symbol(localTable,$1->varRef->id);
		if(t->type->type==1 || t->type->type==2)
		fprintf(yyout,"    iload %d\n",tt->number);
		else if(t->type->type==4)
		fprintf(yyout,"    fload %d\n",tt->number);
		else if(t->type->type==3)
		fprintf(yyout,"    ldc \"%s\"\n",t->attribute->constVal->value.stringVal);
	}
}

			}
			| OP_SUB var_ref
			{
			  if( verifyExistence( symbolTable, $2, scope, __FALSE ) == __TRUE )
			  verifyUnaryMinus( $2 );
			  $$ = $2;
			  $$->beginningOp = SUB_t;

		//printf("++++++++++++++++++%d\n",$2->pType->type);
	if(!isRead){
		struct SymNode* t;struct Symbol* tt;
		t=lookupSymbol(symbolTable,$2->varRef->id,scope,__FALSE);
		if(t->scope==0){//fprintf(yyout,"wheretype: %d\n",t->type->type);
			if(t->type->type==1 || t->type->type==2)
			fprintf(yyout,"    getstatic %s/%s I\n",fileName,$2->varRef->id);
			else if(t->type->type==4)
			fprintf(yyout,"    getstatic %s/%s F\n",fileName,$2->varRef->id);
		}
		else{
			tt=search_for_symbol(localTable,$2->varRef->id);
			if(t->type->type==1 || t->type->type==2)
			fprintf(yyout,"    iload %d\n",tt->number);
			else if(t->type->type==4)
			fprintf(yyout,"    fload %d\n",tt->number);
		}

				if(t->type->type=1)fprintf(yyout,"    ineg\n");
				else if(t->type->type==2)fprintf(yyout,"    ineg\n");
				else if(t->type->type==3){}
				else if(t->type->type==4)fprintf(yyout,"    fneg\n");
}
			}
			| MK_LPAREN boolean_expr MK_RPAREN 
			{
			  $2->beginningOp = NONE_t;
			  $$ = $2; 
			}
			| OP_SUB MK_LPAREN boolean_expr MK_RPAREN
			{
			  //verifyUnaryMinus( $3 );
			  $$ = $3;
			  $$->beginningOp = SUB_t;

			
		if($3->varRef!=0){
			struct SymNode* t;
			t=lookupSymbol(symbolTable,$3->varRef->id,scope,__FALSE);
			//printf("HHHHHHHHHHHHHHH%s\n",$3->varRef->id);
			//printf("HHHHHHHHHHHHHHH%d\n",t->type->type);
			if(t->type->type==1)fprintf(yyout,"    ineg\n");
			else if(t->type->type==2)fprintf(yyout,"    ineg\n");
			else if(t->type->type==3){}
			else if(t->type->type==4)fprintf(yyout,"    fneg\n");
		}
		else{
			if($3->pType->type==1)fprintf(yyout,"    ineg\n");
			else if($3->pType->type==2)fprintf(yyout,"    ineg\n");
			else if($3->pType->type==3){}
			else if($3->pType->type==4)fprintf(yyout,"    fneg\n");
		}

			





			}
			| ID MK_LPAREN opt_boolean_expr_list MK_RPAREN
			{
			  $$ = verifyFuncInvoke( $1, $3, symbolTable, scope );
			  $$->beginningOp = NONE_t;
			  $$->isFunc=1;

	/*struct expr_sem *exprPtr;struct SymNode* t;struct Symbol* tt;
	for( exprPtr=$3 ; (exprPtr->next)!=0 ; exprPtr=(exprPtr->next) ){
	t=lookupSymbol(symbolTable,exprPtr->varRef->id,scope,__FALSE);
	if(t->scope==0){
		if(exprPtr->pType->type==1 || exprPtr->pType->type==2)
		fprintf(yyout,"    getstatic %s/%s I\n",fileName,exprPtr->varRef->id);
		else if(exprPtr->pType->type==4)
		fprintf(yyout,"    getstatic %s/%s R\n",fileName,exprPtr->varRef->id);
	}
	else{
		tt=search_for_symbol(localTable,exprPtr->varRef->id);
		if(exprPtr->pType->type==1 || exprPtr->pType->type==2)
		fprintf(yyout,"    iload %d\n",tt->number);
		else if(exprPtr->pType->type==4)
		fprintf(yyout,"    fload %d\n",tt->number);
	}
		}
	t=lookupSymbol(symbolTable,exprPtr->varRef->id,scope,__FALSE);
	if(t->scope==0){
		if(exprPtr->pType->type==1 || exprPtr->pType->type==2)
		fprintf(yyout,"    getstatic %s/%s I\n",fileName,exprPtr->varRef->id);
		else if(exprPtr->pType->type==4)
		fprintf(yyout,"    getstatic %s/%s R\n",fileName,exprPtr->varRef->id);
	}
	else{
		tt=search_for_symbol(localTable,exprPtr->varRef->id);
		if(exprPtr->pType->type==1 || exprPtr->pType->type==2)
		fprintf(yyout,"    iload %d\n",tt->number);
		else if(exprPtr->pType->type==4)
		fprintf(yyout,"    fload %d\n",tt->number);
	}*/
			  struct SymNode* temp=lookupSymbol(symbolTable,$1,scope,__FALSE);
			  //if(temp==NULL)printf("AAAAAAAAAA\n");
			  //printf("WWWWWWW%d\n",temp->type->type);
			  fprintf(yyout,"    invokestatic %s/%s(",fileName,$1);
			  		int i;
                                        struct PTypeList *pTypePtr;
                                        for( i=0, pTypePtr=(temp->attribute->formalParam->params) ; i<(temp->attribute->formalParam->paramNum) ; i++, pTypePtr=(pTypePtr->next) ) {
                                                //printType( pTypePtr->value, 0 );
						//printf("(%d)",pTypePtr->value->type);

				if(pTypePtr->value->type==1)fprintf(yyout,"I");
				else if(pTypePtr->value->type==2)fprintf(yyout,"Z");
				else if(pTypePtr->value->type==3)fprintf(yyout,"C");
				else if(pTypePtr->value->type==4)fprintf(yyout,"F");
                                                //printf(", ");
					}
					fprintf(yyout,")");

				if(temp->type->type==1)fprintf(yyout,"I\n");
				else if(temp->type->type==2)fprintf(yyout,"Z\n");
				else if(temp->type->type==3)fprintf(yyout,"C\n");
				else if(temp->type->type==4)fprintf(yyout,"F\n");
			}
			| OP_SUB ID MK_LPAREN opt_boolean_expr_list MK_RPAREN
			{
			  $$ = verifyFuncInvoke( $2, $4, symbolTable, scope );
			  $$->beginningOp = SUB_t;
			  $$->isFunc=1;

	/*struct expr_sem *exprPtr;struct SymNode* t;struct Symbol* tt;
	for( exprPtr=$4 ; (exprPtr->next)!=0 ; exprPtr=(exprPtr->next) ){
	t=lookupSymbol(symbolTable,exprPtr->varRef->id,scope,__FALSE);
	if(t->scope==0){
		if(exprPtr->pType->type==1 || exprPtr->pType->type==2)
		fprintf(yyout,"    getstatic %s/%s I\n",fileName,exprPtr->varRef->id);
		else if(exprPtr->pType->type==4)
		fprintf(yyout,"    getstatic %s/%s R\n",fileName,exprPtr->varRef->id);
	}
	else{
		tt=search_for_symbol(localTable,exprPtr->varRef->id);
		if(exprPtr->pType->type==1 || exprPtr->pType->type==2)
		fprintf(yyout,"    iload %d\n",tt->number);
		else if(exprPtr->pType->type==4)
		fprintf(yyout,"    fload %d\n",tt->number);
	}
		}
	t=lookupSymbol(symbolTable,exprPtr->varRef->id,scope,__FALSE);
	if(t->scope==0){
		if(exprPtr->pType->type==1 || exprPtr->pType->type==2)
		fprintf(yyout,"    getstatic %s/%s I\n",fileName,exprPtr->varRef->id);
		else if(exprPtr->pType->type==4)
		fprintf(yyout,"    getstatic %s/%s R\n",fileName,exprPtr->varRef->id);
	}
	else{
		tt=search_for_symbol(localTable,exprPtr->varRef->id);
		if(exprPtr->pType->type==1 || exprPtr->pType->type==2)
		fprintf(yyout,"    iload %d\n",tt->number);
		else if(exprPtr->pType->type==4)
		fprintf(yyout,"    fload %d\n",tt->number);
	}*/
			  struct SymNode* temp=lookupSymbol(symbolTable,$2,scope,__FALSE);
			  //if(temp==NULL)printf("AAAAAAAAAA\n");
			  //printf("WWWWWWW%d\n",temp->type->type);
			  fprintf(yyout,"    invokestatic %s/%s(",fileName,$2);
			  		int i;
                                        struct PTypeList *pTypePtr;
                                        for( i=0, pTypePtr=(temp->attribute->formalParam->params) ; i<(temp->attribute->formalParam->paramNum) ; i++, pTypePtr=(pTypePtr->next) ) {
                                                //printType( pTypePtr->value, 0 );
						//printf("(%d)",pTypePtr->value->type);

				if(pTypePtr->value->type==1)fprintf(yyout,"I");
				else if(pTypePtr->value->type==2)fprintf(yyout,"Z");
				else if(pTypePtr->value->type==3)fprintf(yyout,"C");
				else if(pTypePtr->value->type==4)fprintf(yyout,"F");
                                                //printf(", ");
					}
					fprintf(yyout,")");

				if(temp->type->type==1)fprintf(yyout,"I\n");
				else if(temp->type->type==2)fprintf(yyout,"Z\n");
				else if(temp->type->type==3)fprintf(yyout,"C\n");
				else if(temp->type->type==4)fprintf(yyout,"F\n");
	
			if(temp->type->type==1)fprintf(yyout,"    ineg\n");
			else if(temp->type->type==2)fprintf(yyout,"    ineg\n");
			else if(temp->type->type==3){}
			else if(temp->type->type==4)fprintf(yyout,"    fneg\n");	
			
			}
			| literal_const
			{
			  $$ = (struct expr_sem *)malloc(sizeof(struct expr_sem));
			  $$->isDeref = __TRUE;
			  $$->varRef = 0;
			  $$->isConst = 1;
			  $$->pType = createPType( $1->category );
			  $$->next = 0;
			  if( $1->hasMinus == __TRUE ) {
			  	$$->beginningOp = SUB_t;

				switch( $1->category ) {
				 case INTEGER_t:
				 fprintf(yyout,"    sipush %d\n",$1->value.integerVal);
					break;
				 case REAL_t:
				 fprintf(yyout,"    ldc %f\n",$1->value.realVal);
					break;
				 case BOOLEAN_t:
				 	if($1->value.booleanVal==0)
				 	fprintf(yyout,"    iconst_0\n");
					else if($1->value.booleanVal==1)
					fprintf(yyout,"    iconst_1\n");
					break;
				 case STRING_t:
				 fprintf(yyout,"    ldc \"%s\"\n",$1->value.stringVal);
					break;
				 default:
					/* FIXME */
					break;
			  	}
			  }
			  else {
				$$->beginningOp = NONE_t;			  					
				switch( $1->category ) {
				 case INTEGER_t:
				 fprintf(yyout,"    sipush %d\n",$1->value.integerVal);
				 //fprintf(yyout,"QWQ\n");
					break;
				 case REAL_t:
				 fprintf(yyout,"    ldc %f\n",$1->value.realVal);
					break;
				 case BOOLEAN_t:
				 	if($1->value.booleanVal==0)
				 	fprintf(yyout,"    iconst_0\n");
					else if($1->value.booleanVal==1)
					fprintf(yyout,"    iconst_1\n");
					break;
				 case STRING_t:
				 fprintf(yyout,"    ldc \"%s\"\n",$1->value.stringVal);
					break;
				 default:
					/* FIXME */
					break;
			  	}

			  }



			  	switch( $1->category ) {
				 case INTEGER_t:
				 	$$->value.integerVal=$1->value.integerVal;
					//printf("@@@@@@@@%d\n",$1->value.integerVal);
					break;
				 case REAL_t:
				 	$$->value.realVal=$1->value.realVal;
					break;
				 case BOOLEAN_t:
				 	$$->value.booleanVal=$1->value.booleanVal;
					break;
				 case STRING_t:
				 	$$->value.stringVal=$1->value.stringVal;
					//printf("!!!!!!!!%s",$1->value.stringVal);
					break;
				 default:
					/* FIXME */
					break;
				}
			}
			;

var_ref			: ID
			{
			  $$ = createExprSem( $1 );
			}
			| var_ref dim
			{
			  increaseDim( $1, $2 );
			  $$ = $1;
			}
			;

dim			: MK_LB boolean_expr MK_RB
			{
			  $$ = verifyArrayIndex( $2 );
			}
			;

%%

int yyerror( char *msg )
{
	(void) msg;
	fprintf( stderr, "\n|--------------------------------------------------------------------------\n" );
	fprintf( stderr, "| Error found in Line #%d: %s\n", linenum, buf );
	fprintf( stderr, "|\n" );
	fprintf( stderr, "| Unmatched token: %s\n", yytext );
	fprintf( stderr, "|--------------------------------------------------------------------------\n" );
	exit(-1);
}

