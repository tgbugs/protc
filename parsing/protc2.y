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

%token OPEN CLOSE DEFINE VARIABLE
%token TRUE FALSE NUMBER CHARACTER STRING
%token STEP PROCURE CONCEPT BEING MEASURE /* being and procure do sort of overlap atm */
%token PARAMETERS INPUTS OUTPUTS MEASURES TRANSFORM DESTROY CONDITIONAL EXTERNAL
%token ORDERED REQUIRE_ORD PRACTICAL_ORD
%token REPEAT_FOR REPEAT_FOR_RAND
/*%token FORTOKENS FORTOKENS_RAND FORPARAM FORPARAM_RAND*/

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
variable0: /* empty */
		 | variable0 variable
		 ;
variable: VARIABLE
		;
literal: boolean
	   | number
	   | character
	   | string
 	   /*| quotation*/ /* really? i mean... it is useful for declaring lists but maybe better to skip on that */
	   ;
boolean: true
	   | false
	   ;
true: TRUE
	;
false: FALSE
	 ;
number: NUMBER /* base 1, 2, 8, 10, 16*/
	  ;
character: CHARACTER /* fairley certain this isn't really needed */
		 ;
string: STRING /* yeah... utf8 is no fun.. :( http://www.w3.org/2005/03/23-lex-U */
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
		 | conditional
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
			| conditional /* allows conditionals to be defined in scope if
			needsbe, can do fancy stuff later, may need a conditionals subsection though? */
			;
step_inputs: OPEN INPUTS maybe_ordered_beings0 CLOSE
		   ;
being_nodes: step /* required to have some being output */
		   /*| being */ /* this won't work here unless we change where we can actually define beings or concept -> being transitions*/
		   | variable /* type check elsewhere XXX observe that we can drop being nodes if we type check what the output would be here*/
		   ;
maybe_ordered_beings0: /* empty */
					 | maybe_ordered_beings0 maybe_ordered_beings
					 ;
maybe_ordered_beings: being_nodes
					| order
					;
order: OPEN ordering_rule maybe_ordered_beings0 CLOSE  /* ah orders within orders within orders */
	   /*| OPEN ordering_rule measure0 CLOSE*/
	   ;
ordering_rule: ORDERED
			 | REQUIRE_ORD
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
measure_members: instance_name
			   | measure_type /* */
			   | unit /* how to deal with discrete counts of things here... */
			   ;
instance_name: string
			 /* this is essentially an annotation
			 for what to name something in the output
			 this needs more serious though about how it
			 impacts the structure of an output field and
			 whether we even need this if we are specifying
			 multiple unit measures that go into a measurement struct,
			 something that hasn't been explored, perhaps a measurement
			 specification sublanguage is going to be needed here (probabaly will)
			 */
			 ;
measure_type: variable
			/* having an 'alternate' variable namespace that only
			applies inside a measure definition might work out...
			basically works the opposite of python, when you step
			into a (measure ...) scope there are variables that have
			predefined meanings and are essentially reserved...
			but maybe only in specific slots... requires more thought
			*/
			;
unit: unit_base
	| unit_prefix unit_base /* eh, just lots of translation tables here */
	;
unit_base: VARIABLE /* fix this... might prefer literals in this context or something...
		 need to do w/o ns polution
		 also, need to make sure that the tokenizer doesn't gobble everything here so
		 we *could* define a contextual sub language
		 using VARIABLE directly enables this
		 */
		 ;
unit_prefix: VARIABLE /* as with unit_base */
		   ;
step_outputs: OPEN OUTPUTS maybe_ordered_beings0 CLOSE
			;
step_parameters: OPEN PARAMETERS variable0 CLOSE
			   /* again, the right way to do this needs thought
			   do we allow literals? do we creat a local name binding?
			   only allowing global variables polutes namespaces that may not
			   need to be poluted
			   consider also that this isn't just a variable but a bound variable... (hrm)
			   */
			   ;
concept: OPEN CONCEPT string CLOSE
	   /* import mechanism considerations here again
	   also whether this isnt better handled using types
	   on defined variables or something like that
	   well the implementation here is basically a define-and-type...
	   so maybe this directive will simply serve to accomplish that...
	   */
	   ;
being: OPEN BEING string CLOSE
	 | OPEN BEING variable CLOSE 
	 /* variable needs to refer to a concept,
	 issues w/ promotion from concept->being for restricting
	 valid variables for inputs and outputs...
	 things bound at the 'type' level could simply be
	 put in the global namespace but with a handy prefix or something
	 */
	 ;

external: OPEN EXTERNAL string CLOSE  /* questions of how to deal with imports etc */
		;
conditional: OPEN CONDITIONAL condition_clause0 CLOSE
		   /* if condition is met apply step
		   there are probably some restricutions that need to be satisfied
		   on the inputs of the conditional step
		   */
		   ;
condition_clause0: /* empty */
				 | condition_clause0 condition_clause
				 ;
condition_clause: OPEN condition_test step CLOSE
				| OPEN condition_test expression1 CLOSE /* how is expression1 different from sequence? */
				| OPEN condition_test CLOSE
				/* do we need => here? how would that work on beings?? */
				;
condition_test: expression /* ideally this is what we want... */
			  | OPEN measure_nodes expression CLOSE
			  /* XXX really need to be able to use (and exp1 exp2 exp3) here...
			  I don't think these details need to be part of the grammer and
			  should be implemented in another part of the system...
			  */
			  /* again, binding of names here can be wonky... do the inputs to the
			  measures need to be in scope? (yes) and how should be deal with checking
			  whether it is in scope, it is one thing to just put all the declared inputs
			  in the input list after parsing for a given step, but I think we really want
			  to let people know that they are referring to variable name that is not in
			  in the largest local step scope
			  I can see that within nested step scope we might not bother forcing people
			  to declare inputs if nothing has changed... that might be one way to handle it... HRM
			  BUT if an outer scope did not already define inputs or it is the outer step
			  then inputs are a must...
			  */
			  /* scheme just uses expression here...*/
			  ;
measure_nodes0: /* empty */
			  | measure_nodes0 measure_nodes
			  ;
measure_nodes: measure
			 | variable
			  /* FIXME this needs to go away or be modified, it is not the right way to implement types */
			 ;



/* serious questions about structure here... measures happen inside steps, should
we allow orders on measurers or must those be delegated to step orderings??*/
OLDorder: OLDstep_order /* probably more accurately being order??? */
	 | OLDmeasure_order /* I'm not sure we want to bake the type rule in here?? */
	 ; /* pretty sure this induces massive ambiguity here... unless it is left assoc...? */
OLDstep_order: OPEN maybe_ordered_beings0 CLOSE
		  ;
OLDmeasure_order: OPEN measure0 CLOSE
			 ;
%%
