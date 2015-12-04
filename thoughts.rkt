(defmeasure (measure1 experiment-state)
			; documentation of the measure
			(cons (lambda (value) (cons "name" (cons value volts)))
				  (unspecified-modification-of experiment-state))

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

(procure name1 name2)  ; magically get these and move the being to the namespace of accessible/real/can actually be used beings
(produces step1 name3)  ; move name3 being into the accessible beings namespace but make using name3 pull in a dependency on step1. do we allow name3 to be referenced inside of step1?

(procure (bind name1 "external id 1"))  ; probably won't work given that don't like define in experssion context, which seems like a reasonable idea... though allowing more terse definintions would let you bind the name and procure the being at the same time, which would probably be nice... maybe
(bind-procure name1 "external id 1") ; sure, that might work...

(make-being name1 "external id 1" procure)
(make-being name2 "external id 2" step1)  ; observe here that if this is defined in a block at the start of a protcol step1 is not currently in scope! which sucks but is not surprising...
(make-being name1 (external "eid1") procure) ; validate the external id
(make-being name2 (internal "name2") step1) ; don't try to look up and validate this, makes it simple to 'upgrade' when a real identifier is found...
(make-being name2 "name2" step1) ; don't even need to call anything in this case, since in the end the external function is just a reality check and will return the string or maybe the full IRI at the end

; internal representation format (python style)
{ 
"step name":"step-10",
"executors":1,
"input beings":[bound-being-1, bound-being-2], ; here we will need an imlicity 'everything not listed' variable to allow the measurment target to be ill specified or collective
;"measurement data": [{"name":measurement-1,"unit":volt,"type":float}], ; this should be defined elsewhere, the names just need to be in scope
"measurement input being targets": [{"name":measurement-1, "target":bound-being-1}],  ; measurements should be able to have collective targets, but also bypassing partof makes this more user friendly, so yes membrane potential is a measurement about a circuit made up of a whole bunch of parts of a cell but we really just want to be able to link a measurement to an instance record so we need to let people link directly to the entities that they will be issuing instance identifiers to (eg cell1 cell2 cell3) 'cells have membrane potentials', stuff like that
"output beings":[bound-being-1, bound-being-2], ; this can be inferred in the absense of a destory or transformation function?
}
