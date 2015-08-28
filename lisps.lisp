;; big question: how do we deal with "returning" a value from a
;; real world -> digital function while in a rw-only function?
;; these are HUGE side effects on the "realworld" and they will
;; end up looking like magic and can EASILY get lost like other
;; modifications of global state :/
;; a related issue is whether (and how) to support functions that
;; return both a real world object AND a digital object
;; one way around the problem might be to pass the values measured
;; all the way back out and return a list

(defmacro defrwfun (name args &body body)
  "Define a real world function. These are invariant
  functions that return their invocation when called."
  ; This essentially turns the body into a docstring.
  ; However, since body is still passed in we can do
  ; macrotime syntax validation and scope checking etc.
  ; desired: (learn_english human) -> (learn_english human)
  `(defun ,name (,@args) (list ',name ,@args)))

(defmacro Z (f x &body body)
  `(defun ,f (,@x) (',f ,@x)))

((lambda (name) (lambda (args))))

((lambda (binding-function)
  (lambda (environment)
    (lambda (identifier)
      (lambda (expression)
        (binding-function environment identifier expression)))))
 (bind)(namespace-dict)(name)(some-other-expression))

((lambda (binding-function)
  (lambda (environment)
    (lambda (identifier)
      (lambda (expression)
        (binding-function environment identifier expression))
      (some-other-expression))
    (name))
  (namespace-dict))
(bind))
  
(defparameter human 'human)  ; this is what we want maprw to do

(defrwfun learn_english (human)
  ("Teach your kid english!"))  ; could use type checking on name here but that is going to be tricky since the scoping rules will be insane


; the 9 types of functions we need to deal with:
;   realworld -> realworld
;   realworld -> digital
;   realworld -> (realworld digital)
;   digital -> digital
;   digital -> realworld
;   digital -> (realworld digital)
;   (realworld digital) -> realworld
;   (realworld digital) -> digital
;   (realworld digital) -> (realworld digital)

; THE ABOVE IS FALSE:
; there are 9 types that we need to deal with for
; state modifying functions! we also need to implement
; the full set of functions for being transititions
;   nothing -> being (probably "named form" or something)
;   being -> being (realworld -> realworld)
;   n beings -> m beings (sexual reproduction over time?)
;   being -> n beings (binary fision, budding)
;   n beings -> being (slime mold fusion, sperm/egg)
;   being -> other being (death, ceasing to incorporate matter)
;   being -> nothing (destruction, dispersal, reincorporation)
; all of these types of functions need to have different
; output types because (learn-english human) is not universal
; one ought to wonder whether (sex man woman) should evaluate
; to ((sex man woman) child) or maybe
; (sex man woman) -> (maybe (pregnant woman)))
; (wait (pregnant woman))
; written more clearly:
; [male (range 5 60)mins](defun impregnate (female) ("Woo!"))
; (impregnate female) -> (maybe (impregnate female))
; [universe](defun wait (time subset-of-uniververse)("Time slowly passes..."))
; (visual-inspection-for-signs-of-pregnanacy (wait (months 5) (maybe (impregnate female)))) -> (impregnate female)
; better version: [executor](defun check (maybe-state-thing) (cond ((maybe) maybe-state-thing) ((yes) state-thing) ((no) thing)))

(defun labor-pregnancy (pregnant-female)
  (
  )

(defun organismal-development (environment zygote) (organism)) ; excellent example of where the FULL specification required for complete logical consistency within a language simply is not possible, we CANNOT write this down right now
; somewhere we need a 'representation literal' which maps to some ontology identifier and is acceptable
; I think the code itself works fine for that, there probably isnt really a need for a special type, but we'll see
; basically we need something that can create something out of nothing because we don't actually know all the steps
; and somewhere in there would be a step where we bound a name to the thing or would have classified it as a thing
; ah nominalism

; identifying the executor for many processes is going to be exceedinly hard
; because for many of the processes down near the bottom the agent is nothing
; more than a force, diffusion, gravity, entropy, energy minimization, gradients,
; thermal energy... we really would have to break each function down and identify the
; force that moved each part or oversaw each step
; I think this clearly shows the limits of this model and that it may not be a good
; tool for modeling physical processes since it implies agency where there really is none
; on the other hand adding agency might help people think more clearly about some processes
; since it makes for a better story


[developmental-machinery](defun gestation (pregnant-female)
	    (offspring)) ; where does the description go here?
; this is what we need to deal with because the syntax needs to be clean for specifying the processes involved in gestation
; and that end in live birth, or a 'live born offspring'

(gestation (impregnate female)) ; -> offspring (m or f)


(defmacro defcombout (name args &body body)
  ("data collection code goes here!") ; or maybe... elsehwere?
  `(defun ,name 1))
(defmacro defcombin (name args &body body))
(defmacro defcombinout (name args &body body))




; the bad ones reside below
(defmacro defrealworldfun (function_name parameters body)
  (defun function_name (parameters) (quote (function_name (parameters)))))

(defmacro defrwfun (name args &body body)
  `(defun ,name args (quote (,name args))))

;; notes
;; http://www.cliki.net/case%20sensitivity

