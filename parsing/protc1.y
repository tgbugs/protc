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

in yywrap(){
	return 1;
}
main(){
	yyparse();
}
%}

/* %token IDENTIFIER NUMBER STRING DEFSTEP DEFMEA ASSIGN INPUT OUTPUT ACTUALIZE IMPORT
%token OPENPAREN CLOSEPAREN SINGLEQUOTE DOUBLEQUOTE PERIOD SEMICOLON */

%token DIGIT LETTER SINGLEQUOTE CHARPREFIX NUMBER_DEC SPECIAL_IN SPECIAL_SUB TRUE FALSE ESCESC ESCDBLQUOTE STRELEM CHAR VECTORSTART ELSE EQUALRIGHT DEFINE UNQUOTE UNQUOTE_SPLICING QUOTE LAMBDA IF SETBANG BEGIN COND AND OR CASE LET LETSTAR LETREC DO DELAY QUASIQUOTE
%%

/*lexical structure should probably be moved over to the lex file*/
token:
	 identifier
	 |
	 boolean
	 |
	 number
	 |
	 character
	 |
	 string
	 | '(' | ')' | VECTORSTART | SINGLEQUOTE | '.'
	 ;
delimiter:
		 whitespace
		 | '(' | ')' | '"' | ';'
		 ;
whitespace:
		  space
		  |
		  tab
		  |
		  newline
		  ;
space:
	 ' '
	 ;
tab:
   '\t'
   ;
newline:
	   '\n'
	   ;
comment:
	   ';' all newline
	   ;
all:
   /* empty */
   |
   STRELEM
   |
   '\\'
   |
   '"'
   ;
atmosphere0:
		   /* empty */
		   |
		   atmosphere0 atmosphere
		   ;
atmosphere:
		  whitespace
		  |
		  comment
		  ;
intertoken_space:
				atmosphere0
				;
identifier0:
		   /* empty */
		   |
		   identifier0 identifier
		   ;
identifier:
		  initial
		  |
		  initial subsequent0
		  ;
initial:
	   letter
	   |
	   special_initial
	   ;
letter:
	  LETTER
	  ;
special_initial:
			   SPECIAL_IN
			   ;
subsequent0:
		   /* empty */
		   |
		   subsequent0 subsequent
		   ;
subsequent:
		  initial
		  |
		  digit
		  |
		  special_subsequent
		  ;
special_subsequent:
				  SPECIAL_SUB
				  ;
syntactic_keyword:
				 expression_keyword
				 |
				 ELSE
				 |
				 EQUALRIGHT
				 |
				 DEFINE
				 |
				 UNQUOTE
				 |
				 UNQUOTE_SPLICING
				 ;
expression_keyword:
				   QUOTE
				   |
				   LAMBDA
				   |
				   IF
				   |
				   SETBANG
				   |
				   BEGIN
				   |
				   COND
				   |
				   AND
				   |
				   OR
				   |
				   CASE
				   |
				   LET
				   |
				   LETSTAR
				   |
				   LETREC
				   |
				   DO
				   |
				   DELAY
				   |
				   QUASIQUOTE
				   ;
variable:  /*this isnt quite right*/
		identifier {printf("This is an identifier: %s.\n", $1);}  /* except not syntactic keywords HOW??!? */
		;
digit:
	 DIGIT
	 ;
number:
	  digit
	  |
	  number digit
	  ;
boolean:
	   true | false
	   ;
true:
	TRUE
	;
false:
	 FALSE
	 ;
character:
		 CHARPREFIX CHAR
		 |
		 CHARPREFIX character_name
		 ;
character_name:
			  'space'
			  |
			  'newline'
			  ;
string:
	  '"' string_element0 '"'
	  ;
string_element0:
			   |
			   string_element
			   ;
string_element:
			  STRELEM | ESCDBLQUOTE | ESCESC
			  ;

/* external representations */
datum0:
	  /* empty */
	  |
	  datum0 datum
	  ;
datum1:
	  datum
	  |
	  datum1 datum
	  ;
datum:
	 simple_datum
	 |
	 compound_datum
	 ;
simple_datum:
			boolean
			|
			number
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
	'(' datum0 ')'
	|
	'(' datum1 '.' datum ')'
	|
	abbreviation
	;
abbreviation:
			abbrev_prefix datum
			;
abbrev_prefix:
			 SINGLEQUOTE | '`' | ',' | ',@'
			 ;
vector:
	  VECTORSTART datum0 ')'

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
		 '(quote' datum ')'
		 ;
procedure_call:
			  '(' operator operand0 ')'
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
				 '(lambda' formals body ')'
				 ;
formals:
	   '(' variable0 ')'
	   |
	   variable
	   |
	   '(' variable1 '.' variable ')'
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
definition0:
		   /* empty */
		   |
		   definition0 definition
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
	   ;
conditional:
		   '(if' test consequent alternalte ')'
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
		  '(set!' variable expression ')'
		  ;
derived_expression:
				  '(cond' cond_clause1 ')'
				  |
				  '(cond' cond_clause0 '(else' sequence '))'
				  |
				  '(case' expression case_clause1 ')'
				  |
				  '(case' expression case_clause0 '(else' sequence '))'
				  |
				  '(and' test0 ')'
				  |
				  '(or' test0 ')'
				  |
				  '(let' '(' binding_spec0 ')' body ')'
				  |
				  '(let' variable '(' binding_spec0 ')' body ')'
				  |
				  '(let*' '(' binding_spec0 ')' body ')'
				  |
				  '(letrec' '(' binding_spec0 ')' body ')'
				  |
				  '(begin' sequence ')'
				  |
				  '(do' '(' iteration_spec0 ')' '(' test do_result ')' command0 ')'
				  |
				  '(delay' expression ')'
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
		   '(' test sequence ')'
		   |
		   '(' test ')'
		   |
		   '(' test '=>' recipient ')'
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
		   '((' datum0 ')' sequence ')'
		   ;
binding_spec0:
			 /* empty */
			 |
			 binding_spec0 binding_spec
			 ;
binding_spec:
			'(' variable expression ')'
			;
iteration_spec0:
			   /* empty */
			   |
			   iteration_spec0 iteration_spec
			   ;
iteration_spec:
			  '(' variable init step ')'
			  |
			  '(' variable init ')'
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
		 '(' keyword datum0 ')'
		 ;
keyword:
	   identifier
	   ;
macro_block:
		   '(let-syntax' '(' syntax_spec0 ')' body ')'
		   |
		   '(letrec-syntax' '(' syntax_spec0 ')' body ')'
		   ;
syntax_spec0:
			/* empty */
			|
			syntax_spec0 syntax_spec
			;
syntax_spec:
		   '(' keyword transformer_spec ')'
		   ;

/* quasiquotations NOTE not context free! requires nesting depth variable */
quasiquotation:
			  quasiquotation_d  /* with d = 1 (need to implement) */
			  ;
qq_template_0:
			 expression
			 ;
quasiquotation_d:
				'`' qq_template_d
				|
				'(quasiquote' qq_template_d ')'
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
				  '(' qq_template_or_splice_d0 ')'
				  |
				  '(' qq_template_or_splice_d1 '.' qq_template_d ')'
				  |
				  SINGLEQUOTE qq_template_d
				  |
				  quasiquotation_dp1 /*AAAAAAAAAAAAAAA*/
				  ;
vector_qq_template_d:
					VECTORSTART qq_template_or_splice_d0 ')'
					;
unquotation_d:
			 ',' qq_template_dm1
			 |
			 '(unquote' qq_template_dm1 ')'
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
					  ',@' qq_template_dm1
					  |
					  '(unquote-splicing' qq_template_dm1 ')'
					  ;
qq_template_dm1:
			   /*qq_template_d  /*TODO this needs to decrement something*/
			   ;
quasiquotation_dp1:
				  /*quasiquotation_d  /* TODO this needs to increment something */
				  ;

/* transformers */
transformer_spec:
				'(syntax-rules' '(' identifier0 ')' syntax_rule0 ')'
				;
syntax_rule0:
			/* empty */
			|
			syntax_rule0 syntax_rule
			;
syntax_rule:
		   '(' pattern template ')'
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
	   '(' pattern0 ')'
	   |
	   '(' pattern1 '.' pattern ')'
	   |
	   '(' pattern0 pattern ellipsis ')'
	   |
	   VECTORSTART pattern0 ')'
	   |
	   VECTORSTART pattern0 pattern ellipsis ')'
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
		'(' template_element0 ')'
		|
		'(' template_element1 '.' template ')'
		|
		VECTORSTART template_element0 ')'
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
	   '...'  /* ... is not a valid idenitifer since it starts with '.' ... wut */
	   ;

/* programs and definitions */
program:
	   command_or_definition0
	   ;
command_or_definition0:
					  /* empty */
					  |
					  command_or_definition0 command_or_definition
					  ;
command_or_definition1:
					  command_or_definition
					  |
					  command_or_definition1 command_or_definition
					  ;
command_or_definition:
					 command
					 |
					 definition
					 |
					 syntax_definition
					 |
					 '(begin' command_or_definition1 ')'
					 ;
definition0:
		   /* empty */
		   |
		   definition0 definition
		   ;
definition:
		  '(define' variable expression ')'
		  |
		  '(define' '(' variable def_formals ')' body ')'
		  |
		  '(begin' definition0 ')'
		  ;
def_formals:
		   variable0
		   |
		   variable0 '.' variable
		   ;
syntax_definition:
				 '(define-syntax' keyword transformer_spec ')'
				 ;
%%
