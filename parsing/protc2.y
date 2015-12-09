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
%token STEP PROCURE MEASURE ANALYSIS /* measure is ambigious here, it really means define-measure */
%token CONCEPT BEING /* these are more type declarations and might impl as such */
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
		 | analysis /* implies that analysis here is essentially a struct like the rest... */
		 | order /* not sure this is where this needs to go... */
		 | repeat
		 | conditional
		 | procure
		 | destroy
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
			| analysis /* i think this needs to be here but from outside it looks more like a measure */
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
			   /* code or pointer to code that actually does the measuring... */
			   /* somewhere in the sub language we need a way to state whether
			   the measure produces a symbol that IS the data or simply POINTS TO
			   the data, if it POINTS TO the data we should provide a second set
			   of fields that hold the code for interpreting it or accessing it
			   or a pointer to that code
			   this will become important down the line when thinking about
			   linking (analysis ...) steps more cleanly, though that will
			   come much later since once you have the output of protc the
			   biggest gap will be closed... the only issue might be when one
			   wants to do an online analysis step during the running of a protocol
			   in theory that could be treated as a measure step with respect to
			   the local experiment context... but measure: being->number and
			   anaysis is really analysis: number->parameter
			   */
			   ;
instance_name: variable /* THIS IS REALLY A DEFERRED NAME */
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
unit_base: variable /* fix this... might prefer literals in this context or something...
		 need to do w/o ns polution
		 also, need to make sure that the tokenizer doesn't gobble everything here so
		 we *could* define a contextual sub language
		 using VARIABLE directly enables this
		 */
		 ;
unit_prefix: variable /* as with unit_base */
		   ;
step_outputs: OPEN OUTPUTS maybe_ordered_beings0 CLOSE
			;
step_parameters: OPEN PARAMETERS variable0 CLOSE /* looked up or defined params would match this... */
			   | OPEN PARAMETERS deferred_name0 CLOSE  /* pretty sure these ALL refer to deferred... */
			   /* again, the right way to do this needs thought
			   do we allow literals? do we creat a local name binding?
			   only allowing global variables polutes namespaces that may not
			   need to be poluted
			   consider also that this isn't just a variable but a bound variable... (hrm)
			   */
			   ;
concept: OPEN CONCEPT string CLOSE  /* this is more a type declaration than anything... */
	   /* import mechanism considerations here again
	   also whether this isnt better handled using types
	   on defined variables or something like that
	   well the implementation here is basically a define-and-type...
	   so maybe this directive will simply serve to accomplish that...
	   */
	   ;
being: OPEN BEING string CLOSE /* this is also a type declaration about the subset of concepts, implying that it has tokens in the world */
	 /*| OPEN BEING variable CLOSE */
	 /* variable needs to refer to a concept,
	 issues w/ promotion from concept->being for restricting
	 valid variables for inputs and outputs...
	 things bound at the 'type' level could simply be
	 put in the global namespace but with a handy prefix or something
	 */
	 ;
procure: OPEN PROCURE variable CLOSE
	   | OPEN PROCURE expression CLOSE
	   /* variable must have concept type being should raise
	   type error already actualized... though actualized is
	   probably not the term we want for this process since
	   that happens when we actually _run_ the protocol */
	   /* expression must produce something w/ concept type...
	   which means that the type of the output needs to be
	   resolvable at syntax time... so we need to add function
	   types so that things like (concept ...) and (being ...)
	   are known to produce concepts or beings and dont simply
	   have expression types (as would be implied by OPEN CLOSE)
	   */
	   ;
destroy: OPEN DESTROY variable0 CLOSE
	   /* variable type needs to resolve to an actualized being type (confusing)
	   similarly we probably want to be able to destroy full expressions as well
	   or the subset of them that produce actualized beings...
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
			 | variable /* if this references analysis it will fix deferred_name issues */
			  /* FIXME this needs to go away or be modified, it is not the right way to implement types */
			 ;

OLDanalysis: OPEN ANALYSIS OPEN OLDanalysis_nodes0 CLOSE body CLOSE
		/*| OPEN ANALYSIS analysis_parameters analysis_outputs body CLOSE*/
		/* analysis is a first class function like step
		the difference is that where step takes beings as inputs
		(technically also takes measures and parameters as well)
		analysis only takes parameters or things that produces
		parameters like measures
		its structure is similar to step in that it will accept
		inputs and allow outputs but in the middle instead of measures
		there is real code
		outputs essentially functions as a way to declare the structure of
		the output of the analysis so that it can be understood and consumed
		more easily than a traditional s-expression
		if outputs is omitted then the parameters returned will be anonymous
		ALL of the above requires quite a bit more thought and consideration
		*/
		;
analysis: OPEN ANALYSIS aparams1 aoutputs OPEN body CLOSE CLOSE  /* not sure this is right :/*/
		| OPEN ANALYSIS aparams1 aoutputs interpreter OPEN body CLOSE CLOSE
		| OPEN ANALYSIS aparams1 aoutputs interpreter file CLOSE
		/* further contemplation suggests that
		analysis is much more like a measure
		than a step since like measure analysis
		produces parameters, the difference being
		that analysis should also accept parameters
		as inputs while measurements should only
		accpet beings, figuring out how to ordering
		analysis steps and sections without resorting
		to the usual nesting seems like it could be
		tricky... though if using named parameter inputs
		then that can be resolved by simply sticking it in
		after all the measures that produce those parameters
		have been successfully completed
		be mindful however, that while analysis certainly does
		have steps, that those steps do not correspond to the
		steps defined here which are used to manage the input
		and output of beings, internally analysis should allow
		the use of any language, if lang is defined, otherwise
		it should just work like lisp with no issues
		*/
		/* even more thinking revels that analysis sections could appear in
		the measures section OR the parameters section of steps :/ annoying
		but perhaps useful in the long run, though it will confuse the hell
		out of people to have to deal with anonymous analysis functions that
		take a named parameter or named measure (or collection of both! utoh)
		and transform them into something that is used in another context
		but without a name... (seems like the is actually unlikely to happen...)
		though we might want to enable it since it does seem like it could be useful
		*/
		;
aparams1: aparams 
		| aparams1 aparams 
		;
aparams: deferred_name
	   | measure
	   ;
deferred_name0:
			 | deferred_name0 deferred_name
			 ;
deferred_name: variable 
			 /* this works in partnership with instance_name
			 to enable safe referencing and tracking of measure
			 names
			 */
			 ;

aoutputs: OPEN variable0 CLOSE
		/* these are the same things that measures output
		(basically quoted names that only become real at run time)
		since their naming is deferred we want to let people write
		normal runtime code that isn't quoted all to hell
		the easiest way to do this is to just require people
		to specify the names and manage those in a special namespace
		where we can emit warnings if names are overwritten and the like
		and ALERT when a consumer of a name would be schedule before that
		name was actualized (crap... there's our ambiguity right there)
		due to how nesting and order requirements might lead to impossible
		occurences
		ALTERNATELY: we could enforce strong ordering restrictions by requiring
		that steps EITHER be analysis steps or measurement steps so that
		we can gurantee that measurements bind those names before analysis
		try to access them
		*/
		;
interpreter: string
		   ;
body: expression /* gonna need some extra to make sure expression matches lang, etc */
	;
file: string /* consider file/path literals... iirc CL had this? */
	;

OLDanalysis_nodes0: /* empty */
			   | OLDanalysis_nodes0 OLDanalysis_nodes
			   ;
OLDanalysis_nodes: analysis
			  | variable /* cannot accept measure directly in current pattern because then analysis must accept beings as inputs... which actually seems reaonsable... shoot */
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
