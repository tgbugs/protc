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
 
; THE ABOVE IS ALSO -FALSE- incomplete (heh)
;   nothing -> being (transitions occur once per universe!)
;   being -> nothing (conservation of energy seems to imply that this does not happen!)
;   the transitions I was trying to document above are actually about named vs unnamed collections of matter/energy
;
;   energy-matter -> named-being
;   named-being -> named-being  (here energy-matter on both sides is ignored because it cancels)
;   n named-beings -> m named-beings  (this IS possible, consider (sperm egg) -> zygote case, makes matching numbers hard
;   named-being -> named-being n named-beings
;
;   named-being energy-matter -> n named-beings  ( this is actually the same as below...)
;   named-being -> n named-beings  (this implies breaking into parts and the destruction of the original being)
;
;   named-being -> energy-matter  (death, or at least, things no longer considered in the protocol)
;   named-being 
;
;   energy-matter, named-being, n named-being, conservative transition, destructive transition
;   energy-matter named-being -> energy-matter named-being
;   n-b1 n-b2 -> n-b1   (a steak is indeed a named being, all named-beings are also energy-matter!) DERP
;   therefore the types of functions need to be delineated more clearly
;
;   creation
;   destruction, disassembly
;   incorporation  (a steak that I eat is incorporated into my body)
;   subsumption   (eg when mitochondria were incorporated)
;   mixing
;   transformation  (these are the normal state transition functions)
;   
;   (allowing multiple transitions only via energy-matter seems... unnesscary???)
;   for example one way to handle reference counting of inputs and outputs is to
;   require all n -> m transitions to operate via energy-matter
;   named-being -> energy-matter -> named-being1 named-being2
;   but this is clearly a destructive transition and we will have to deal with
;   the number of parts changing anyway :/
;   OR we just require a call to creation every time and a 'divide into n parts' call... HRM
;   we *could* enable reference counting/matching by forcing any transitions to produce collections?
;   but what about when a reagent is consumed! DERP, incorporation functions would only have one output
;
;   really all we want to be able to do here is track what came from where
;   this mapping SHOULD be possible to infer automatically based on inputs and outputs
;   BUT that requries that the programmer write down all the things that are actually output
;   at the end, including the executor and the tools, (sometimes the executor will NOT be output at the end :/ )
;   which is a pain in the ass unless we have some way to communicate automatically based on positional values
;   what things are to be consumed and which are conserved...
;
;   implicit vs explicit executors...
;
;   I think the best way to deal with creation and destruction is to have inner functions where
;   the call to create within them explicity means that ONLY the other things in the name space
;   are used for creation... and if something does not come out the other end it accounts for the
;   pieces that were created
;   now, one way to make sure we have provenance is to require that ALL functions take only ONE input
;   so that we can track the provenance, for example a dissection takes energy and a mouse but
;
;   any call to create shall occur inside a function that has only a single input <- this seems like it might work
;   note that create has a VERY specific meaning here and includes an implicit assertion that the thing
;   created has met the criteria for being named as such (recall the car example where we must have a
;   function that specifies how we know it is a car "honks like a car")

(defun make-something (energy-matter) (create something))
(defun more-complicated (energy-matter tools)
  ((lambda (energy-matter) (list (create something)
			    (create something-else)))
   (energy-matter)))

(defun process (executors things-conserved things-consumed) (executors things-conserved-or-transformed))


(defun slice-prep-disection (executor mouse tools) (executor (brain-on-a-block mouse-parts) tools-less-block-and-some-glue-and-some-agarose)


(defun sex (male female) (list (tired male) (maybe (impregnate female))))
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

(defun create (thing) (quote thing) ("A shorthand for dealing with the fact that we can't
		       document the full process for creating some beings")) ; again, need syntax here
(defun create (thing)
  "Shorthand to assert that by some unspecified process
  thing comes into being. Implicit consumption of energy
  here. Should it be made explicity?"
  (quote thing))

(defun create (energy-matter thing_name) (quote thing-name))

(defun destroy (thing-name) (quote energy-matter)) ; use energy-matter to denote all transitions that are not the universe coming into being
(defun dispose-of (thing-name) nil)  ; notation for making things that we don't need/want to deal with in this protocl anymore go away, implicitly invokes reincorporation, actually as long as all we are requiring is that the number of named things entering a function is the same as the number leaving a function... we *should* be able to handle it automatically tools being an irritating case since sometimes you can reuse them and other times you need to dispose/destroy them for safety reasons (surgical tools for example), i suppose we could default to automatcially writing in (put-away thing) for anything that is not explicitly destroyed or disposed of...

[developmental-machinery](defun gestation (pregnant-female)
	    (create offspring)) ; where does the description go here?
; this is what we need to deal with because the syntax needs to be clean for specifying the processes involved in gestation
; and that end in live birth, or a 'live born offspring'
; NOTE was missing the (create thing) function...

; FIXME the above function represents a very unhappy event of death in childberth!
; having the different types of functions handle this automatically might be one way
; to avoid having to do a bunch of typing (see the readme on conservation of mass)
; so if there is no consumer function that removes a being (really we can't specify the full process of reincorporation)
; ALSO you need to add energy! derp! don't want that to be implicit but it is going to be a pain to write
; the following all the time:
(defun continue-existing '(energy being) '(being waste))

(gestation (impregnate female)) ; -> offspring (m or f)


(defmacro defcombout (name args &body body)
  ("data collection code goes here!") ; or maybe... elsehwere?
  `(defun ,name 1))
(defmacro defcombin (name args &body body))
(defmacro defcombinout (name args &body body))


; concurrent execution of multiple protocols and contingencies
; all top level protocols shall take no inputs and executor shall be implicit or in the annotation
(exec-concurrent mouse-surgery if-mouse-bleed if-need-to-sneeze if-cut-self if-fire-alarm if-big-list-of-rare-events
		 if-you-must-stop-or-leave-for-any-reason-here-is-what-you-must-do)  ; FIXME this is wrong
; the only exec-concurrent for the attentive agent that is the human being executing this protocol is the mouse surgury

[human-brain continual](exec-concurrent breath keep-heart-beating attend-to-stuff coordinate-movement plan-future-movement)
[human 15 min](play-piano-piece-from-memory some-piece)
(defun play-piano-piece-from-memory (some-piece) (exec-concurrent sequence-of-left-hand-movements sequence-of-right-hand-movements sequence-of-right-foot-movements synchornization))  ; oh man... this is crazy we can't actually specify this!


; it seems like we need a scheduler like feature, an event-loop like feature, and an interrupt handler
; worst of all the interrupt handler is going to be state dependent!
; see if we really need this... these may be features we can add later if we plan for them in advance...

(defun if-big-list-of-rare-events ()  ; note, does not handle simultaneous occurance of rare events
  (if (detect occurance) (what-to-do) dont-worry-about-it)
  (if (detect air-strike)  ; need some way to substitute/expand these with a full protocol...
      ((if (detect mushroom-cloud)  ; probably better NOT to use nested constructs here... except that these are really conds... because the action you take will be mutually exclusive and you MUST prioritize and sequence... INTERESTING...
	   (kiss your-ass) ; goodbye
	   (take-cover location)))
      dont-worry-about-it)
  (if (detect rabid-squirrel)
      (run fast)
      dont-worry-about-it)
  (if (detect smoke)
      (move-yourself somewhere-outside-with-fresh-air)
      dont-worry-about-it)))

; really the implementation should use cond... but even that is a bit weird because there there will be other things that need to condinue (such as breathing)... event loop???
; basically how do we handle interrupts and denote things that can progress simultaneously vs those that mutex or block


; the bad ones reside below
(defmacro defrealworldfun (function_name parameters body)
  (defun function_name (parameters) (quote (function_name (parameters)))))

(defmacro defrwfun (name args &body body)
  `(defun ,name args (quote (,name args))))

;; notes
;; http://www.cliki.net/case%20sensitivity

