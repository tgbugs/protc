;#lang racket ; THIS WILL BREAK IMPORTS IF UNCOMMENTED
(require plot)
(require (for-syntax racket/match))
(require (for-syntax syntax/parse)) ; super duper uper useful >_< AND COMPLETELY BROKEN w/ typed/racket
(require (for-syntax macro-debugger/stepper-text))
(begin-for-syntax ; _ and ... will cause things to screech if syntax/parse is not available...

  (define-syntax-class sexp
    (pattern (_ ...)))

  (define-syntax-class expr-or-id
	(pattern (_ ...))
	(pattern _:id))

)

(define-syntax (def stx)
  (syntax-parse stx
	[(_ x ...) #'(define x ...)])) ; wheeee

(define-syntax (dip stx)
  (syntax-parse stx
	[(_ name) #'(define name (symbol->string 'name))]  ; this feels so dirty wat
	[(_ name ident) #'(define name ident)]))

;(define-syntax (type stx)
  ;(syntax-parse stx
	;[(_ name) #'(lambda (value) (cons name value))]))

(define-syntax (type-stx stx)
  (syntax-parse stx
	[(_ int:integer) #''integer]
	[(_ num:number) #''number]
	[(_ id:id) #'(cdr id)]))

(define (type thing)
  (cond [(integer? thing) 'integer]
		[(flonum? thing) 'flonum]
		[(number? thing) 'number]
		[(string? thing) 'string]
		[(pair? thing) (cdr thing)]))

(define (type-bad thing)
  (case (eval thing)  ; this doesnt work :/
	[(integer? (eval thing)) 'integer]
	[(string? (eval thing)) 'string?]
	[(flonum? (eval thing)) 'flonum]
	[(number? (eval thing)) 'number]
	))

(define-syntax (type-name-def stx)
  (syntax-parse stx
	;[(type-name name) #'(define name ((type 'type-name) (symbol->string 'name)))]))
	[(_ type-name name identifier)
	 #'(define name (cons identifier 'type-name))]
	[(_ type-name name) ; how do I macro this for multiple syntax level objects? (eg measure)
	 #'(define name (cons (symbol->string 'name) 'type-name))]))

(define-syntax (being stx)
  (syntax-parse stx
	[(name x ...) #'(type-name-def name x ...)]))
(define-syntax (measure-alt stx) ; alternate form for measures not using struct
  (syntax-parse stx
	[(name x ...) #'(type-name-def name x ...)]))

(dip zoop "bar") zoop
(dip foo) foo
(dip are-you-pooping-me) are-you-pooping-me

(define-syntax (step-sequence stx)
  (syntax-parse stx
	[(_ steps:sexp ...+) #'(list steps ...)]
	[(_ steps:id ...+) #'(list steps ...)])) ; we will need facilities to do the full expansion here
											 ; will also need to validate inputs/outputs at each step

(define-syntax (step stx)
  ; TODO: this MUST return an object that we can call (run-step) or (display-step) or (flatten-step) on... something like that
  ; this needs significant refinement to properly match input measure output etc
  (syntax-parse stx
    [(_ i:sexp m:sexp o:sexp) #'(list i m o)]
    [(_ i:sexp m:sexp) #'(list i m)]
    [(_ i:sexp o:sexp) #'(list i o)]
    [(_ i:sexp) #'(list i)]
    [(_ o:sexp) #'(list o)]))

(define-syntax (transform stx)
  (syntax-parse stx
	[(_ (bound-ids:id ...) new-name)
	 #'new-name])) ; need to insert the prov tracking here, OR use the nesting reference macro...
	 ;#'((define new-name (symbol->string 'new-name)(new-name)))])) ; pretty sure new-name here will break if we use define, will need our own namespace :(

(define-syntax (silly stx)
  (match (syntax->datum stx)
	[(list _ name) (datum->syntax stx
								  `(define-syntax (,name stx)
									 (syntax-parse stx
									   [(_ ei:expr-or-id ...+)
										#'(list ei ...)])))]))

(define-syntax (inputs stx)
  ;(expand/step-text stx)
  (syntax-parse stx
    [(_ ei:expr-or-id ...+) #'(list ei ...)]))  ; ideally would use something more restrictive than expr
(define-syntax (measures stx)
  (syntax-parse stx
    [(_ ei:expr-or-id ...+) #'(list ei ...)]))
(define-syntax (outputs stx)
  (syntax-parse stx
    [(_ ei:expr-or-id ...+) #'(list ei ...)]))

;(struct measure ([name-tag : String] [prefix : String] [unit : Unit]) ; the docs show just how fun this could be...
(struct measure (name-tag prefix unit) #:property prop:custom-write (lambda (v p w?) (fprintf p "<~a, ~a~a>" (measure-name-tag v) (measure-prefix v) (measure-unit v)))) ; this is bad: we want sub structure to be fexible here

;(define-type Unit (U String))
;(step 1 2 3)
(dip woo)
(dip ten 10)
(dip your-mom)
(dip sean-connery)
(dip your-mom-ko)
(dip number-of-deaths) ; this isn't really a unit... :/
;(: volt Unit)  ; this stuff needs to autoimport
(dip volt)
;(: joule Unit)
(dip joule)
(dip mili)
(dip mega)
(being foo)
(dip m-id-bad) ; FIXME not a valid measure...
(define m-id-good (measure "tag me this way baby" mili volt ))
;(inputs (dip you-wot-m8)); observe the fail as desired

(inputs woo) ; valid but shouldn't do anything outside this context?
(outputs ten)
(define step-1 (step (inputs your-mom)
	  (measures (measure "ohoho" mili volt) m-id-bad m-id-good)
	  (outputs (transform (your-mom) your-mom-ko)))
)
(step
  ; this describes what is done in this step in words
  ; if needs be, so we understand the madness?
  (inputs (step (measures (measure "bongos" mega joule)) ; observe the anonymous step here... it will need to be assigned a name somehow
				(inputs woo)))
  (measures (measure "are you sure about this?" "" number-of-deaths))
  (outputs sean-connery))

(step-sequence
  (step (inputs foo)))


#|
(defmeasure (measure1 experiment-state)
			; documentation of the measure
			(cons (lambda (value) (cons "name" (cons value volts)))
				  (unspecified-modification-of experiment-state)))

; with syntactic sugar
(defme (measure1 experiment-state)
	   ; woo!
	   (actualizes "name-tag" volts))  ; volts would be bound to an id elsewhere

(defun (actualizes name unit) ; NOTE this implementation fails because experiment-state is not in scope, probably need syntax
  (cons (lambda (value) (cons name (cons value unit)))  ; essentially creates a nameless function to actualize the measure... could confound debug?
		(unspecified-modification-of experiment-state)))

(experiment
  (inputs "a" "b" "c")
  (outputs "d" "e" "f")
  (measurements (measure1 "a") (measure2 "b") (measure3 "c")) ; observe that (measure1 "a") returns an anonymous function that does the measuring but cannot easily be tracked or identified without introspection to the 'name' it returns :/
  ; it seems that we would like to bind the instances of real measurement events to names locally (eg making (measurements () () ()) a macro that calls (let a -> ()) or something so that when we run the code to collect the data the names display properly
  (bindings))

(define exp1 (experiment (inputs (name1 name2 name3)
					(measurements (m1 name1) (m2 name2) (m3 name3)))))
(run exp1)

(define (run experiment)
  ; takes the output of an experiment defintion and interperts it
  ; so that steps are provided and inputs are taken from the command line
  ; this does not make use of executor rules that could be used to hint
  ; where one ought to source the data from, since STDIN isnt alwasy
  ; the correct place...
  (do-steps (parse-to-linear-sequence-of-steps experiment)))

(define (parse-to-linear-sequence-of-steps experiment)
  ; doesn't have to be linear... if there are multiple executors
  ; there is still a linear sequence of coordination steps that
  ; the 'overseeing' process will have to execute or set of conditions
  ; that it must wait for, these should be derivable from the spec
  ; using the number of executors and dependency rules on steps
  ; if you are indeed using a coordinator/ 'overseer' then we
  ; probably want to be able to serilize overseer state so
  ; that long running processes can be relocated
  ())

; experiment definitions need to bind data-acquisition fucntions to steps...
; behind the scenes, the executors are for documentation purposes...


; namespaces for beings
(define-name-bindings (name1 name2 name3) ("external id 1" "eid2" "eid3"))

(procure name1 name2)  ; magically get these and move the being to the namespace of accessible/real/can actually be used beings FIXME doesn't this break strong typing!? either way we want to make sure the concept -> 'being' transition is handled transparently while still retaining sensible typing (the specification language should not have explicit user accessible types)
(produces step1 name3)  ; move name3 being into the accessible beings namespace but make using name3 pull in a dependency on step1. do we allow name3 to be referenced inside of step1?

(procure (bind name1 "external id 1"))  ; probably won't work given that don't like define in experssion context, which seems like a reasonable idea... though allowing more terse definintions would let you bind the name and procure the being at the same time, which would probably be nice... maybe
(bind-procure name1 "external id 1") ; sure, that might work...

(make-being name1 "external id 1" procure)
(make-being name2 "external id 2" step1)  ; observe here that if this is defined in a block at the start of a protcol step1 is not currently in scope! which sucks but is not surprising...
(make-being name1 (external "eid1") procure) ; validate the external id
(make-being name2 (internal "name2") step1) ; don't try to look up and validate this, makes it simple to 'upgrade' when a real identifier is found...
(make-being name2 "name2" step1) ; don't even need to call anything in this case, since in the end the external function is just a reality check and will return the string or maybe the full IRI at the end

; internal representation format (python style)
;	{ 
;	"step name":"step-10",
;	"executors":1,
;	"input beings":[bound-being-1, bound-being-2], ; here we will need an imlicity 'everything not listed' variable to allow the measurment target to be ill specified or collective
;	;"measurement data": [{"name":measurement-1,"unit":volt,"type":float}], ; this should be defined elsewhere, the names just need to be in scope
;	"measurement input being targets": [{"name":measurement-1, "target":bound-being-1}],  ; measurements should be able to have collective targets, but also bypassing partof makes this more user friendly, so yes membrane potential is a measurement about a circuit made up of a whole bunch of parts of a cell but we really just want to be able to link a measurement to an instance record so we need to let people link directly to the entities that they will be issuing instance identifiers to (eg cell1 cell2 cell3) 'cells have membrane potentials', stuff like that
;	"output beings":[bound-being-1, bound-being-2], ; this can be inferred in the absense of a destory or transformation function?
;	}

; need the ability to use measurements as contingencies, even if the measurement requires a human brain to check 'is the mouse bleeding profuesly' which is a boolean, and isn't something we want to record... we we need a way to flag unrecorded measurements that are required to keep track of experiment state and specify contingencies...
; how do congingencies affect building dags? I think they are just interrupts... and the stack needs to return to where it was...
; also need a way to implement timers/restrictions on state... I think this works fine using measurements (eg maintain a continuous measure of time since start)
; might want a simple way to show assertions of 'equivalent' measurements, eg that slices go 'bad' after X amount of time, so just use time instead of doing the full 'is it bad yet' function

; high level syntax structures that we need to define
number-executors ; not sure this is needed???
	(number-executors integer)
concept
	(concept name "identifier")
actualize
	procure ; is a special kind of step? bypasses the need to retrieve steps that produce the inputs you need...
		(procure concept-name) ; DISLIKE imlicit statefulness here, but what are you going to do? also type restirction?
	output
	produces
		(produces step-name concept-name)
	transform
		(transform being-name-1 being-name-2)
step
	(step step-name
		  (inputs being-name-1 being-name-2)
		  (measurements (measure-1 being-name-1))) ; existing table of know measurements???
measurement
conditional
sequential
	(sequential step-name-1 step-name-2)
nonsequential
	(nonsequential step-name-1 step-name-2 step-name-3)


; dealing w/ more terse define syntax
(define (tool name)) ; this works but requires the use of a second (define t-var-1 (tool name))
(define (tool name [identifier '()]))





; i should be using define-syntax-rule here instead...? or not...
(define-syntax (define-in-place stx) ; this works in inner scope but does not register the name in global scope
  (match (syntax->list stx)
	[(list name variable)
	 ;(print stx)
	 ;(print (list name variable))
	 ;(datum->syntax stx `(define ,variable ,(format "~s" (syntax->datum variable))))]  ; THAT WAS EASY :D woo format
	 (datum->syntax stx `(define ,variable ,(symbol->string (syntax->datum variable))))] ; alternate format
	[(list name variable identifier)
	 (datum->syntax stx `(define ,variable ,identifier))]))

(define-syntax (new-in-place stx)
  (match (syntax->list stx)
	[(list name new-stx)
	 (datum->syntax stx)]
	[]))

(syntax-parse #'(define a 10)
  #:literals (define)
  [(define var:id body:expr) 'ok])

(define global-things (make-hash))
(define (add-thing! name val)
  (hash-set! global-things name val))

(define-syntax define-in-place
  (syntax-rules
	() ; things to be treated as literals in the following
	[(define-in-place variable) ; this version doesn't work because i need access to variable??
	 (define variable (format "~s" variable))] ; doesnt work because the id IS bound in the template :/
	[(define-in-place variable identifier)
	 (define variable identifier)]))

; WHEEEE tom is an idiot from time to time: closer to what we want: struct! maybe? no?
(struct step (inputs measures outputs))  ; but.... wait... this confuses things... immensely

(define step-state '())
(define (step inputs measures outputs)
  ; validate
  (define (validate-input input) ())
  (define (validate-measure measure) ())
  (define (validate-output output) ())
  (list (map validate-input inputs)
		(map validate-measure measures)
		(map validate-output outputs)))

; a functional style for declaring protocols, as opposed to a procedural...
; man this would be easier with types...
(define m-variable-1 (measure "nametag" amp (using b-tool-variable-1))) ; probably useful to support nametag uniqueness
; need to make sure we can resolve tools as step inputs when declared inside a (using var)... hrm
; tools should also be able to be collections, eg a rig, that can start out as a single reference and go from there
(define s-variable-1
  (step (inputs b-variable-1
			   (step (inputs b-variable-3) ; the seems to break the suggestion that substeps have restricted input scope?
					 					; but not quite because those variables occur before the outer step finishes
										; and an all be listed posthoc as inputs to step-name-1
										; observe also that the inner step here lacks details, beyond 'transform'
					 (transform (b-variable-3) b-variable-4) ; transform or create or procure required here?
					 (outputs b-variable-4))
			   b-variable-2)
		(measures m-variable-1
				 (measure "nametag" volt)
				 m-variable-2)
		(outputs variable+)))	; output must be -parsed-validated last i think and can be omitted in many cases
|# ; END COMMENT BLOCK

