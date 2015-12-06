/*
	actually an r5rs impl...
	from: https://people.csail.mit.edu/jaffer/r5rs_9.html
*/
%{
#include <stdio.h>
#include <string.h>

void yyerror(const char *str){
	fprintf(stderr, "error: %s\n", str);
}

int yywrap(){
	return 1;
}
main(){
	yyparse();
}
%}

%token OPEN
%token CLOSE
%token DOT
%token IDENTIFIER
%token VARIABLE
%token NUMBER
%token TRUE
%token FALSE
%token STRING
%token CHARACTER
%token SINGLEQUOTE
%token VECTORSTART
%token COMMAT
%token QUOTE
%token LAMBDA
%token IF
%token SETBANG
%token BEGIN_
%token COND
%token AND
%token OR
%token CASE
%token LET
%token LETSTAR
%token LETREC
%token DO
%token DELAY
%token QUASIQUOTE
%token ELSE
%token EQUALRIGHT
%token DEFINE
%token UNQUOTE
%token UNQUOTE_SPLICING
%token LET_SYNTAX
%token LETREC_SYNTAX
%token SYNTAX_RULES
%token DEFINE_SYNTAX
%token ELLIPSIS

%%
/* programs and definitions */
program:
	   command_or_definition0
	   {printf("Hello world\n");}
	   ;
command_or_definition0:
					  /* empty */
					  {printf("cmd or def 0 zero\n");}
					  |
					  command_or_definition0 command_or_definition
					  {printf("cmd or def 0 more than zero\n");}
					  ;
command_or_definition1:
					  command_or_definition
					  {printf("cmd or def 1 one\n");}
					  |
					  command_or_definition1 command_or_definition
					  {printf("cmd or def 1 more than one \n");}
					  ;
command_or_definition:
					 command
					 {printf("cmd or def command\n");}
					 |
					 definition
					 {printf("cmd or def definition\n");}
					 |
					 syntax_definition
					 {printf("cmd or def syntax\n");}
					 |
					 OPEN BEGIN_ command_or_definition1 CLOSE
					 {printf("cmd or def begin\n");}
					 ;
definition0:
		   /* empty */
		   {printf("definition0 empty\n");}
		   |
		   definition0 definition
		   {printf("definition0 defintion\n");}
		   ;
definition:
		  OPEN DEFINE variable expression CLOSE
		  {printf("definition\n");}
		  |
		  OPEN DEFINE OPEN variable def_formals CLOSE body CLOSE
		  |
		  OPEN BEGIN_ definition0 CLOSE
		  ;
def_formals:
		   variable0
		   |
		   variable0 DOT variable
		   ;
syntax_definition:
				 OPEN DEFINE_SYNTAX keyword transformer_spec CLOSE
				 ;

/* lexical binding */
identifier0:
		   /* empty */
		   |
		   identifier0 identifier
		   ;
identifier:
		  IDENTIFIER
		  ;
variable:
		VARIABLE
	  	{printf("variable\n");}
		;
number:
	  NUMBER
	  {printf("number %d\n", $1);}
	  ;
boolean:
	   TRUE
	   {printf("boolean TRUE\n");}
	   |
	   FALSE
	   {printf("boolean FALSE\n");}
	   ;
string:
	  STRING
	  ;
character:
		 CHARACTER
		 ;

/* external representations */
datum0:
	  /* empty */
	  {printf("datum0 empty\n");}
	  |
	  datum0 datum /* for some reason d before d0 improves parse selection*/
	  {printf("datum0 datum\n");}
	  ;
datum1:
	  datum
	  {printf("datum1 datum\n");}
	  |
	  datum1 datum
	  {printf("datum1 datum1 datum\n");}
	  ;
datum:
	 simple_datum
	 {printf("datum simple_datum\n");}
	 |
	 compound_datum
	 ;
simple_datum:
			boolean
			|
			number
			{printf("simiple_dataum number\n");}
			|
			character
			|
			string
			|
			symbol
			;
symbol:
	  identifier
	  ;
compound_datum:
			  list
			  |
			  vector
			  ;
list:
	OPEN datum0 CLOSE
	{printf("list datum0\n");}
	|
	OPEN datum1 CLOSE  /* because sometimes the parser is dumb */
	|
	OPEN datum1 DOT datum CLOSE
	{printf("why are you parsing to here ;_;\n");}
	|
	abbreviation
	;
abbreviation:
			abbrev_prefix datum
			;
abbrev_prefix:
			 SINGLEQUOTE | '`' | ',' | COMMAT
			 ;
vector:
	  VECTORSTART datum0 CLOSE

/* expressions */
expression:
		  variable
		  |
		  literal
		  |
		  procedure_call
		  |
		  lambda_expression
		  |
		  conditional
		  |
		  assignment
		  |
		  derived_expression
		  |
		  macro_use
		  |
		  macro_block
		  ;
literal:
	   quotation
	   {printf("literal quotation\n");}
	   |
	   self_evaluating
	   ;
self_evaluating:
			   boolean
			   |
			   number
			   |
			   character
			   |
			   string
			   ;
quotation:
		 SINGLEQUOTE datum
		 |
		 OPEN QUOTE datum CLOSE
		 ;
procedure_call:
			  OPEN operator operand0 CLOSE
			  ;
operator:
		expression
		;
operand0:
		|
		operand0 operand
		;
operand:
	   expression
	   ;
lambda_expression:
				 OPEN LAMBDA formals body CLOSE
				 ;
formals:
	   OPEN variable0 CLOSE
	   |
	   variable
	   |
	   OPEN variable1 DOT variable CLOSE
	   ;
variable0:
		 /* empty */
		 |
		 variable0 variable
		 ;
variable1:
		 variable
		 |
		 variable1 variable
		 ;
body:
	definition0 sequence
	;
sequence:
		command0 expression
		;
command0:
		/* empty */
		|
		command0 command
		;
command:
	   expression
	   {printf("command\n");}
	   ;
conditional:
		   OPEN IF test consequent alternalte CLOSE
		   ;
test0:
	 /* empty */
	 |
	 test0 test
	 ;
test:
	expression
	;
consequent:
		  expression
		  ;
alternalte:
		  /* empty */
		  |
		  expression
		  ;
assignment:
		  OPEN SETBANG variable expression CLOSE
		  ;
derived_expression:
				  OPEN COND cond_clause1 CLOSE
				  |
				  OPEN COND cond_clause0 OPEN ELSE sequence CLOSE CLOSE
				  |
				  OPEN CASE expression case_clause1 CLOSE
				  |
				  OPEN CASE expression case_clause0 OPEN ELSE sequence CLOSE CLOSE
				  |
				  OPEN AND test0 CLOSE
				  |
				  OPEN OR test0 CLOSE
				  |
				  OPEN LET OPEN binding_spec0 CLOSE body CLOSE
				  |
				  OPEN LET variable OPEN binding_spec0 CLOSE body CLOSE
				  |
				  OPEN LETSTAR OPEN binding_spec0 CLOSE body CLOSE
				  |
				  OPEN LETREC OPEN binding_spec0 CLOSE body CLOSE
				  |
				  OPEN BEGIN_ sequence CLOSE
				  |
				  OPEN DO OPEN iteration_spec0 CLOSE OPEN test do_result CLOSE command0 CLOSE
				  |
				  OPEN DELAY expression CLOSE
				  |
				  quasiquotation
				  ;
cond_clause0:
			/* empty */
			|
			cond_clause0 cond_clause
			;
cond_clause1:
			cond_clause
			|
			cond_clause1 cond_clause
			;
cond_clause:
		   OPEN test sequence CLOSE
		   |
		   OPEN test CLOSE
		   |
		   OPEN test EQUALRIGHT recipient CLOSE
		   ;
recipient:
		 expression
		 ;
case_clause0:
			/* empty */
			|
			case_clause0 case_clause
			;
case_clause1:
			case_clause
			|
			case_clause1 case_clause
			;
case_clause:
		   OPEN OPEN datum0 CLOSE sequence CLOSE
		   ;
binding_spec0:
			 /* empty */
			 |
			 binding_spec0 binding_spec
			 ;
binding_spec:
			OPEN variable expression CLOSE
			;
iteration_spec0:
			   /* empty */
			   |
			   iteration_spec0 iteration_spec
			   ;
iteration_spec:
			  OPEN variable init step CLOSE
			  |
			  OPEN variable init CLOSE
			  ;
init:
	expression
	;
step:
	expression
	;
do_result:
		 /* empty */
		 |
		 sequence
		 ;
macro_use:
		 OPEN keyword datum0 CLOSE
		 ;
keyword:
	   identifier
	   ;
macro_block:
		   OPEN LET_SYNTAX OPEN syntax_spec0 CLOSE body CLOSE
		   |
		   OPEN LETREC_SYNTAX OPEN syntax_spec0 CLOSE body CLOSE
		   ;
syntax_spec0:
			/* empty */
			|
			syntax_spec0 syntax_spec
			;
syntax_spec:
		   OPEN keyword transformer_spec CLOSE
		   ;

/* quasiquotations NOTE not context free! requires nesting depth variable */
quasiquotation:
			  quasiquotation_d  /* with d = 1 (need to implement) */
			  ;
qq_template_0: /* HRM also needs implementation */
			 expression
			 ;
quasiquotation_d:
				'`' qq_template_d
				|
				OPEN QUASIQUOTE qq_template_d CLOSE
				;
qq_template_d:
			 simple_datum
			 |
			 list_qq_template_d
			 |
			 vector_qq_template_d
			 |
			 unquotation_d
			 ;
list_qq_template_d:
				  OPEN qq_template_or_splice_d0 CLOSE
				  |
				  OPEN qq_template_or_splice_d1 DOT qq_template_d CLOSE
				  |
				  SINGLEQUOTE qq_template_d
				  |
				  quasiquotation_dp1 /*AAAAAAAAAAAAAAA*/
				  ;
vector_qq_template_d:
					VECTORSTART qq_template_or_splice_d0 CLOSE
					;
unquotation_d:
			 ',' qq_template_dm1
			 |
			 OPEN UNQUOTE qq_template_dm1 CLOSE
			 ;
qq_template_or_splice_d0:
					  /* empty */
					  |
					  qq_template_or_splice_d0 qq_template_or_splice_d
					  ;
qq_template_or_splice_d1:
					  qq_template_or_splice_d
					  |
					  qq_template_or_splice_d1 qq_template_or_splice_d
					  ;
qq_template_or_splice_d:
					 qq_template_d
					 |
					 splicing_unquotation_d
					 ;
splicing_unquotation_d:
					  COMMAT qq_template_dm1
					  |
					  OPEN UNQUOTE_SPLICING qq_template_dm1 CLOSE
					  ;
qq_template_dm1:
			   /*qq_template_d  /*TODO this needs to decrement something*/
			   ;
quasiquotation_dp1:
				  /*quasiquotation_d  /* TODO this needs to increment something */
				  ;

/* transformers */
transformer_spec:
				OPEN SYNTAX_RULES OPEN identifier0 CLOSE syntax_rule0 CLOSE
				;
syntax_rule0:
			/* empty */
			|
			syntax_rule0 syntax_rule
			;
syntax_rule:
		   OPEN pattern template CLOSE
		   ;
pattern0:
		/* empty */
		|
		pattern0 pattern
		;
pattern1:
		pattern
		|
		pattern1 pattern
		;
pattern:
	   pattern_identifier
	   |
	   OPEN pattern0 CLOSE
	   |
	   OPEN pattern1 DOT pattern CLOSE
	   |
	   OPEN pattern0 pattern ellipsis CLOSE
	   |
	   VECTORSTART pattern0 CLOSE
	   |
	   VECTORSTART pattern0 pattern ellipsis CLOSE
	   |
	   pattern_datum
	   ;
pattern_datum:
			 string
			 |
			 character
			 |
			 boolean
			 |
			 number
			 ;
template:
		pattern_identifier
		|
		OPEN template_element0 CLOSE
		|
		OPEN template_element1 DOT template CLOSE
		|
		VECTORSTART template_element0 CLOSE
		|
		template_datum
		;
template_element0:
				 /* empty */
				 |
				 template_element0 template_element
				 ;
template_element1:
				 template_element
				 |
				 template_element1 template_element
				 ;
template_element:
				template
				|
				template ellipsis
				;
template_datum:
			  pattern_datum
			  ;
pattern_identifier:
				  identifier  /* need to exclude `...' */
				  ;
ellipsis:
	   ELLIPSIS  /* ... is not a valid idenitifer since it starts with '.' ... wut */
	   ;
%%
