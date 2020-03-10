%{
	#include <stdlib.h>
	#include <stdio.h>
	#include <string.h>
	#include "symb.h"

	char current_dtype[100];
	int yyerror(char *msg);
	int current_value;
	char current_id[100];
	char buff[100];
	int error=0;
	int array=0;
	int decl = 0;
	int add_to_table = 0;
	node1 * tree;
		
%}


%union
{
	struct values{
		int ival;
		double dval;
		char name[100];
		char d_type[100];
		node1 * nd;	
	}val;
}

/*defining typees */
%type <val> starter builder function declaration single_stmt compound_stmt statements stmt sub_decl declaration_list 
%type <val> if_block while_block for_block expression_stmt array_index function_call arg arguments argument_list

%type <val> expression
%type <val> sub_expr
%type <val> constant
%type <val> unary_expr
%type <val> arithmetic_expr
%type <val> assignment_expr
%type <val> lhs //needs to get name
%type <val> assign_op
%type <val> type_specifier
%type <val> data_type
%type <val> type // top 3 need to be d_type
%token <val> T_IDENTIFIER //name

 /* Constants */
%token <val> T_DEC_CONSTANT //ival
%token <val> T_FLOAT_CONSTANT //dval
%token T_CHAR_CONSTANT
%token T_STRING_LITERAL

 /* Logical and Relational operators */
%token T_LOGICAL_AND T_LOGICAL_OR T_LS_EQ T_GR_EQ T_EQ T_NOT_EQ T_LT

 /* Short hand assignment operators */
%token T_MUL_ASSIGN T_DIV_ASSIGN T_MOD_ASSIGN T_ADD_ASSIGN T_SUB_ASSIGN
%token LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN
%token T_INCREMENT T_DECREMENT

 /* Data types */
%token <val> T_INT 
%token <val> T_CHAR 
%token <val> T_FLOAT 
%token <val> T_VOID //all need to be d_type

 /* Keywords */
%token T_IF T_FOR T_WHILE CONTINUE BREAK T_RETURN T_printff T_scanff

/*extras */
%token SIGNED UNSIGNED
%token T_SEMICOLON T_OPENFL T_CLOSEFL T_OPENFUNC T_CLOSEFUNC
%token T_PLUS T_SUB T_MUL T_DIV T_MOD  T_GT T_ASSIGN T_NOT T_COMMA T_DIMS T_OPENSQ T_CLOSESQ
%token T_INVALID


%start starter

%left T_COMMA
%right T_ASSIGN
%left T_LOGICAL_OR
%left T_LOGICAL_AND
%left T_EQ T_NOT_EQ
%left T_LT T_GT T_LS_EQ T_GR_EQ
%left T_PLUS T_SUB
%left T_MUL T_DIV T_MOD
%right T_NOT


%nonassoc UMINUS
%nonassoc LOWER_THAN_ELSE
%nonassoc T_ELSE

%%

 /* Program is made up of multiple builder blocks. */
starter: starter builder {tree = $$.nd; $$.nd = mknode($1.nd, $2.nd, "root"); /*printf("\nAST1\n:"); printtree($$.nd);*/}
	|builder { $$.nd = mknode(NULL, $1.nd, "starter");}
	|error T_SEMICOLON{ printf("Doing panic mode recovery. . ."); $$.nd = mknode(NULL, NULL, "error"); }
	;

 /* Each builder block is either a function or a declaration */
builder: function {$$.nd = mknode(NULL, $1.nd, "builder1"); }
       |declaration {$$.nd = mknode(NULL, $1.nd, "builder2"); }
       |single_stmt {$$.nd = mknode(NULL, $1.nd, "builder3");}
       ;

 /* This is how a function looks like */
function: type T_IDENTIFIER {$2.nd = mknode(NULL, NULL, $2.name); 
			    char * to_send[100]; 
			    strcpy(to_send,$1.d_type);
			    strcat(to_send, strdup("_function"));
			    add_func($2.name,to_send,print_line()); 
			    } T_OPENFUNC argument_list T_CLOSEFUNC compound_stmt 
		{node1 *dummy = mknode($5.nd, $7.nd , " "); $$.nd = mknode($2.nd, dummy, "function");}
	;

 /* Now we will define a grammar for how types can be specified */

type :
    data_type {strcpy($$.d_type,$1.d_type); printf("\nin type: %s\n", $$.d_type);}
    ;

data_type :
    type_specifier {strcpy($$.d_type,$1.d_type); printf("\nin data_type: %s\n", $$.d_type);}
    ;


type_specifier : T_INT  {strcpy($$.d_type,$1.d_type); printf("data_type: %s", $$.d_type); strcpy(current_dtype, $$.d_type);}
	| T_CHAR  {strcpy($$.d_type,$1.d_type); printf("data_type: %s", $$.d_type); strcpy(current_dtype, $$.d_type);}
	| T_FLOAT  {strcpy($$.d_type,$1.d_type); printf("data_type: %s", $$.d_type); strcpy(current_dtype, $$.d_type);}
	| T_VOID   {strcpy($$.d_type,$1.d_type); printf("data_type: %s", $$.d_type); strcpy(current_dtype, $$.d_type);}
    ;

 /* grammar rules for argument list */
 /* argument list can be empty */
argument_list :arguments
    |
    ;
 /* arguments are comma separated TYPE ID pairs */
arguments :arguments T_COMMA arg {node1 *dummy = mknode($3.nd, $1.nd, ","); $$.nd = mknode(NULL, dummy, "parameters");}
    |arg {$$.nd = $1.nd;}
    ;

 /* Each arg is a TYPE ID pair */
arg :type T_IDENTIFIER {$$.nd = mknode($1.nd, NULL, $2.name);}
   ;

 /* Generic statement. Can be compound or a single statement */
stmt:compound_stmt  {$$.nd = mknode(NULL, $1.nd, "comp_statement");}
    |single_stmt {$$.nd = mknode(NULL, $1.nd, "single_statement");}
    ;

 /* The function body is covered in braces and has multiple statements. */
compound_stmt : T_OPENFL statements T_CLOSEFL {$$.nd = mknode(NULL, $2.nd, "compound_statement");}
    ;

statements:statements stmt {$$.nd = mknode($1.nd, $2.nd, "new statement");}
    |
    ;

 /* Grammar for what constitutes every individual statement */
single_stmt :if_block
    |for_block
    |while_block
    |declaration
    |function_call T_SEMICOLON
    |T_RETURN T_SEMICOLON
    |CONTINUE T_SEMICOLON
    |BREAK T_SEMICOLON
    |T_RETURN sub_expr T_SEMICOLON
    |T_printff T_OPENFUNC T_STRING_LITERAL T_CLOSEFUNC T_SEMICOLON 
    |T_printff T_OPENFUNC T_STRING_LITERAL T_COMMA var T_CLOSEFUNC T_SEMICOLON 
    ;

var: T_IDENTIFIER
    | T_IDENTIFIER T_COMMA var
    ;

for_block:T_FOR T_OPENFUNC expression_stmt expression_stmt T_CLOSEFUNC stmt {    
    	node1 *y = mknode($4.nd, NULL, "Y_DUMMY");
    	node1 *x = mknode($3.nd, y, "X_DUMMY");
    	$$.nd = mknode(x, $6.nd, "for");
    }
    |T_FOR T_OPENFUNC expression_stmt expression_stmt expression T_CLOSEFUNC stmt 
    {    
    	node1 *y = mknode($4.nd, $5.nd, "Y_DUMMY");
    	node1 *x = mknode($3.nd, y, "X_DUMMY");
    	$$.nd = mknode(x, $7.nd, "for");
    }
    ;

if_block:T_IF T_OPENFUNC expression T_CLOSEFUNC stmt %prec LOWER_THAN_ELSE {$$.nd = mknode($3.nd,$5.nd,"if");}
	|T_IF T_OPENFUNC expression T_CLOSEFUNC stmt T_ELSE stmt { node1* dummy = mknode($3.nd, $5.nd, "if"); $$.nd = mknode(dummy,$7.nd,"else");}
    ;

while_block: T_WHILE T_OPENFUNC expression T_CLOSEFUNC stmt {$$.nd = mknode($3.nd, $5.nd, "while");}
		;

declaration: type {decl=1; $1.nd = mknode(NULL, NULL, $1.d_type);add_to_table = 1; array = 1;} declaration_list T_SEMICOLON {$$.nd = mknode($1.nd, $3.nd, "declaration1"); decl=0;}
			 | declaration_list T_SEMICOLON {$$.nd = mknode(NULL, $1.nd, "declaration2");}
			 | unary_expr T_SEMICOLON
             ;

declaration_list: declaration_list {add_to_table++; printf("IM HAPPENING\n");}T_COMMA sub_decl {$$.nd = mknode($1.nd, $4.nd, ",");}
		| sub_decl {$$.nd = mknode(NULL, $1.nd, "declaration_list");}
		;  

sub_decl: assignment_expr {printf("\nassignment_expr: %lf\n", $1.dval); $$.nd = mknode(NULL, $1.nd, "assignment");}
    |T_IDENTIFIER      {strcpy(current_id, $1.name); insert_id(); add_to_table--; $$.nd = mknode(NULL, NULL, $1.name);}              
    |array_index   
    ;

/* This is because we can have empty expession statements inside for loops */
expression_stmt:expression T_SEMICOLON {$$.nd = mknode(NULL, $1.nd, "expression_stmt");}
    |T_SEMICOLON {$$.nd = mknode(NULL, NULL, ";");}
    ;

expression:
    expression T_COMMA sub_expr	 {$$.nd = mknode($1.nd, $3.nd, ",");}							
    |sub_expr { $$.dval = $1.dval; $$.nd = mknode(NULL, $1.nd, "expression");}
    ;

sub_expr:
    sub_expr T_GT sub_expr	        {$$.nd = mknode($1.nd, $3.nd, ">");}				
    |sub_expr T_LT sub_expr	        {$$.nd = mknode($1.nd, $3.nd, "<");}						
    |sub_expr T_EQ sub_expr		{$$.nd = mknode($1.nd, $3.nd, "==");}				
    |sub_expr T_NOT_EQ sub_expr         {$$.nd = mknode($1.nd, $3.nd, "!=");}        
    |sub_expr T_LS_EQ sub_expr          {$$.nd = mknode($1.nd, $3.nd, "<=");}        
    |sub_expr T_GR_EQ sub_expr          {$$.nd = mknode($1.nd, $3.nd, ">=");}        
    |sub_expr T_LOGICAL_AND sub_expr    {$$.nd = mknode($1.nd, $3.nd, "&&");}       
    |sub_expr T_LOGICAL_OR sub_expr     {$$.nd = mknode($1.nd, $3.nd, "||");}      
    |T_NOT sub_expr                     {$$.nd = mknode(NULL, $2.nd, "!");}         
    |arithmetic_expr {$$.dval = $1.dval; /*$$.nd = mknode(NULL, $1.nd, "arithmetic_expression");*/}							
    |assignment_expr {$$.nd = mknode(NULL, $1.nd, "assignment_expression");}           
    |unary_expr	     {$$.nd = mknode(NULL, $1.nd, "unary_expression");}
    |type assignment_expr {$$.nd = mknode(NULL, $2.nd, $1.d_type); insert_id();} 
    ;


assignment_expr :lhs assign_op arithmetic_expr { 
		
		$$.nd = mknode($1.nd, $3.nd, $2.nd->token);

		$$.dval = $3.dval; 
		strcpy(current_id, $1.name); 
		printf("!!!%d", add_to_table); 
		if(add_to_table < 0) add_to_table=0;
		else {
			add_to_table--; 
			if(decl==1) {
			insert_id();
			}		
		}
		gcvt($$.dval, 10, buff); 
		printf("\nlhs: %s\tassignment_expr : %s\n", $1.name, buff); 
		int res = search_update($1.name, buff, current_dtype);
			if(res == -1) {
				printf("\nError at Line no: %d: Variable doesn't exist", print_line());
			}
		}
    |lhs assign_op array_index         {$$.nd = mknode($1.nd, $3.nd, $2.nd->token);}                 
    |lhs assign_op function_call       {$$.nd = mknode($1.nd, $3.nd, $2.nd->token);}            
    |lhs assign_op unary_expr          {$$.nd = mknode($1.nd, $3.nd, $2.nd->token);}            
    |unary_expr assign_op unary_expr   {$$.nd = mknode($1.nd, $3.nd, $2.nd->token);}           
    ;

unary_expr:	
	lhs T_INCREMENT       {$$.nd = mknode($1.nd, NULL, "++");}                   
	|lhs T_DECREMENT      {$$.nd = mknode($1.nd, NULL, "--");}                           
	|T_DECREMENT lhs      {$$.nd = mknode(NULL, $2.nd, "++");}                           
	|T_INCREMENT lhs      {$$.nd = mknode(NULL, $2.nd, "--");}                          

lhs:T_IDENTIFIER { strcpy($$.name, $1.name); $$.nd = mknode(NULL, NULL, $1.name); }
    ;

assign_op:T_ASSIGN  {$$.nd = mknode(NULL, NULL, "=");}                                      
    |T_ADD_ASSIGN   {$$.nd = mknode(NULL, NULL, "+=");}                                  
    |T_SUB_ASSIGN   {$$.nd = mknode(NULL, NULL, "-=");}                                  
    |T_MUL_ASSIGN   {$$.nd = mknode(NULL, NULL, "*=");}                                  
    |T_DIV_ASSIGN   {$$.nd = mknode(NULL, NULL, "/=");}                                  
    |T_MOD_ASSIGN   {$$.nd = mknode(NULL, NULL, "%=");}                                  
    ;
    

arithmetic_expr: arithmetic_expr T_PLUS arithmetic_expr  {$$.nd = mknode($1.nd, $3.nd, "+");}  
    |arithmetic_expr T_SUB arithmetic_expr               {$$.nd = mknode($1.nd, $3.nd, "-");}      
    |arithmetic_expr T_MUL arithmetic_expr               {$$.nd = mknode($1.nd, $3.nd, "*");}      
    |arithmetic_expr T_DIV arithmetic_expr               {$$.nd = mknode($1.nd, $3.nd, "/");}      
    |arithmetic_expr T_MOD arithmetic_expr               {$$.nd = mknode($1.nd, $3.nd, "%");}
    |T_OPENFUNC arithmetic_expr T_CLOSEFUNC              {$$.nd = mknode(NULL, $2.nd, "()");} 
    |T_SUB arithmetic_expr %prec UMINUS                  {$$.nd = mknode(NULL, $2.nd, "unary -");} 
    |T_IDENTIFIER   {$$.nd = mknode(NULL, NULL, $1.name);}                                    
    |constant      {
    			$$.dval = $1.dval; 
    			printf("\nconstant: %lf\n", $$.dval); 
			gcvt($1.dval, 10, buff);
    			$$.nd = mknode(NULL, NULL, buff);
    		   }                              
    ;

constant: T_DEC_CONSTANT {$$.dval = $1.ival; printf("$$ is: %lf\n", $$.dval);}                               
    | T_FLOAT_CONSTANT   {$$.dval = $1.dval; printf("$$ is: %lf\n", $$.dval);}                               
    | T_CHAR_CONSTANT	 
    ;

array_index: T_IDENTIFIER T_OPENSQ sub_expr T_CLOSESQ { 
			strcpy(current_id, $1.name); 
			current_value = $3.dval; 
			if(array == 1) {
				insert_id();
				printf("\nsize of array: %lf", $3.dval); 
				printf("\nin declaration: %s\n", $1.name); 
				//strcpy(current_dtype, $1); 
				gcvt(current_value, 10, buff);
				strcat(current_dtype, strdup("_array"));
				int res = search_update(current_id, buff, current_dtype);
				if(res == -1) {
					printf("Error at line no: %d: Variable does not exist", print_line());
				}
			}
			array = 0;
			
		}
	;

function_call: T_IDENTIFIER T_OPENFUNC parameter_list T_CLOSEFUNC
             |T_IDENTIFIER T_OPENFUNC T_CLOSEFUNC
             ;

parameter_list:
              parameter_list T_COMMA  parameter
              |parameter
              ;

parameter: sub_expr
	|T_STRING_LITERAL

        ;
%%

#include "lex.yy.c"
#include <ctype.h>

int main(int argc, char *argv[])
{
	
	yyin = fopen(argv[1], "r");
	

	if(!yyparse() && !error)
	{
		printf("\nParsing complete\n");
	}
	else
	{
		printf("\nParsing failed\n");
	}

	fclose(yyin);
	display();
	printf("\nAST\n:"); printtree(tree);
	return 0;
}

int yyerror(char *msg)
{
	printf("Line no: %d Error message: %s Token: %s\n", yylineno, msg, yytext);	
}

int print_line() {
	return yylineno;
}

void insert_id() {
		strcpy(buff, strdup("N/A"));
		printf("\nKJASNDKJSAND: %s %s\n", current_id, current_dtype); 
		int res = search_update_decl(current_id, current_dtype, print_line()); 
		if(res == -1) {
			printf("Redeclaration Error at line no %d\n", print_line()); 
		}
}
