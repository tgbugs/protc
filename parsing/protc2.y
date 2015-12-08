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

%token STEP INPUTS OUTPUTS MEASURES

%%

step_def:
		STEP step_structure
		;
step_structure:
			  inputs_def
			  |
			  outputs_def /* not sure we really want to allow this */
			  |
			  inputs_def measures_def
			  |
			  inputs_def measures_def outputs_def
			  |
			  inputs_def outputs_def
			  ;
inputs_def:
		  INPUTS inputs_structure1
		  ;
inputs_structure:
		  variable /* ideally w/ added type info... */
		  |
		  step_def
		  ;

outputs_def:
		   variable
		   |
		   transform_def
		   |
		   destropy_def
		   ;
measuresd_def:
			 instace_name_tag
			 |
			 
			 ;



/* thinking below */

/*
TERMINALS:
		 (
		 )
		 VARIABLE
		 TRUE
		 FALSE
		 STRING
		 NUMBER

nonterminals:
			step_declaration
			measure_declaration
			being_declration

step_declaration:
				OPEN STEP step_subsection0 CLOSE
				;
step_subsection0:
				/* empty */
				|
				step_subsection0 step_subsection
				;
step_subsection:
			   explantation
			   |
			   variable  /* in theory we could use types here but it confuses */
			   |
			   declration
			   ;


step_subsection_:
			   explanation
			   |
			   input
			   |
			   measure
			   |
			   output
			   ;
input:
	 variable /* with the correct type... checked elsewhere */
	 |
	 being_declaration
	 ;

expression:
		  variable
		  |
		  literal
		  |
		  declaration
		  ;

declaration:
		   OPEN operator operand0 CLOSE
*/

%%
