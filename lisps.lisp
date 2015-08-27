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
