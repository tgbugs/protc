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

%token OPEN CLOSE STEP PARAMETERS INPUTS OUTPUTS MEASURES TRANSFORM CONDITIONAL DEFINE PROCURE CONCEPT EXTERNAL
/*%token FORTOKENS FORTOKENS_RAND FORPARAM FORPARAM_RAND*/
%token REPEAT_FOR REPEAT_FOR_RAND
%token ORDERED REQUIRE_ORD PRACTICAL_ORD

%%

specification: definition_or_expression0
			 ;
definition_or_expression0: /* empty */
						 | definition_or_expression0 definition_or_expression
						 ;
definition_or_expression: definition
						| expression
						;
definition: OPEN DEFINE variable expression CLOSE
		  ;
expression0: /* empty */
		   | expression0 expression
		   ;
expression1: expression
		   | expression1 expression
		   ;
expression: variable
		  | literal
		  | directive
		  ;
directive1: directive
		  | directive1 directive
		  ;
directive: step /* these are top level directives, being might at some point be able to become a lower level if we change define restrictions */
		 | being /* subclasses can be useful here, also can't use being here directly! */
		 | concept
		 | external /* kind of like import...? but resolves identifiers... probably need other/better framework for this... */
		 | measure
		 | order /* not sure this is where this needs to go... */
		 | repeat
		 ;
step: OPEN STEP step_members0 CLOSE
	;
step_members0: /* empty */
			 | step_members0 step_members
			 ;
step_members: step_inputs /* substeps could be inferred from the ordering of inputs? */
			| step_measures
			| step_outputs
			| step_parameters
			;
step_inputs: OPEN INPUTS being_nodes0 CLOSE
		   | OPEN INPUTS ordered0 being_nodes0 CLOSE
		   ;
being_nodes0: /* empty */
			| being_nodes0 being_nodes
			;
being_nodes: step /* required to have some being output */
		   /*| being */ /* this won't work here unless we change where we can actually define beings or concept -> being transitions*/
		   | variable /* type check elsewhere */
		   ;
order: OPEN ordering_rule being_nodes0 CLOSE
	   /*| OPEN ordering_rule measure0 CLOSE*/
	   ;
ordering_rule: ORDERED
			 | REQUIRED_ORD
			 | PRACTICAL_ORD
			 ;
repeat: OPEN repeat_rule variable0 step CLOSE  /* variables need to refer to beings that are inputs to the step or lists of parameters, interestingly here the parameters are one of the things that can be specified ahead of time, even though they are actually tokens of a parameter type in the enclosed step... HRM the parameters could be collected at runtime for a given instance since they usually specify what was done, not what could have been done... supplying syntax for providing the runtime variables might be a good idea here...*/

	  /* do we allow (let ([local-param-set-name '(1 2 3 4)])) because '(1 2 3 4)
	  ain't gonna work unless theres a name to bind it to inside the params section :/
	  this situation is going to come up again... we need to find an elegant way to solve it
	  */
	  ;

repeat_rule: REPEAT_FOR   /* translation and flattening of this is going to be super fun! */
		   | REPEAT_FOR_RAND /* a the eternal confusion between what was doen and what should have been done */
		   /*| FORTOKENS
		   | FORTOKENS_RAND
		   | FORPARAM
		   | FORPARAM_RAND
		   */
		   ;
step_measures: OPEN MEASURES measure0 CLOSE /* treat all measures as simultaneous... but we have vectors for that...
			 also (step (measures m2 (step (measures m3)))) sucks, as does dealing with loops and repeats
			 (which is gonna be SUPER fun to work out how to do correctly when translating from types->tokens)
			 this could also be dealt with at the outputs step since steps can live in outputs
			 is a (repeat-for-tokens being-variable (step (inputs being-variable) ...)) directive sufficient?
			 or (repeat-for-cartesian-randomly being-variable-1 being-variable-2 (step (inputs being-variable-1 being-variable-2)))
			 */
			 ;
measure0: /* empty */
		| measure0 measure
		;
measure: OPEN MEASURE measure_members CLOSE
	   ;
step_outputs:



/* serious questions about structure here... measures happen inside steps, should
we allow orders on measurers or must those be delegated to step orderings??*/
order: step_order /* probably more accurately being order??? */
	 | measure_order /* I'm not sure we want to bake the type rule in here?? */
	 ; /* pretty sure this induces massive ambiguity here... unless it is left assoc...? */
step_order: OPEN being_nodes0 CLOSE
		  ;
measure_order: OPEN measure0 CLOSE
			 ;


/* old below */

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
