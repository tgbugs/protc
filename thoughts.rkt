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

