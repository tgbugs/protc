;in xrepl use ,en "test.rkt" or (enter! "test.rkt")
#lang racket
(require racket/enter)  ; drracket can't find module->exports w/o this
(require (for-syntax racket/match))
(require (for-syntax syntax/parse)) ; super duper uper useful >_< AND COMPLETELY BROKEN w/ typed/racket

; UNITS... amazing how much easier it is when langs have this accessible
; except for the fact that it doesn't quite fit our use case ;_;
;(require measures-with-dimensions) ; raco pkg install measures-with-dimensions
;(require measures-with-dimensions/chemistry)

;; useful racket code
; define env
(define (env)
  (map (lambda (x)
	 (let ([y (bytes->string/utf-8 x)])
	   (cons y (getenv y))))
       (environment-variables-names (current-environment-variables))))

; define who

(define rkt-base
  (append
    '(rkt-base)
    (map car (cdar (let-values ([(x y) (module->exports 'racket/enter)]) y)))
    (map car (let-values ([(x y) (module->exports 'racket)])
			 (append (cdaddr x) (cdaddr y))))))

(define (who)
  (let ([all-symbols (namespace-mapped-symbols (current-namespace))])
    (sort (filter (lambda (term) (not (member term rkt-base))) all-symbols) symbol<?)))


;; experimental code

; FIXME something about the way this is defined causes weirdness with match and referring to defrw independently also doesn't give bad syntax errors like it should, needs more work
(define-syntax (defrw stx)  ; FIXME need to support (name arg . args) syntax here
  ; TODO do we want this to handle stuff like checking for defined renamings/synonyms?
  (match (syntax->datum stx)
    [(list _ name-args body)
     ; add code to validate the body
     (displayln (cons "TODO:" body))
     (match name-args
       [(list-rest name rest) (datum->syntax stx `(define ,name-args (list ',name ,@rest)))])]))

;(define-syntax (executor stx))  ; TODO
(define (executor name) "this needs to work like typed racket")

;(define-syntax (executor-namespace stx))  ; TODO
(define (executor-namespace name verbs-nouns) "verbs and nouns should probably be split")

;(define-syntax (def-realworld->digital stx))  ; TODO
(define (def-realworld->digital expression measurement-rule) "measure stuff or other rw->digital transforms")

(defrw (hello thing1 thing2) ("A BODY"))


; we need a variant of defrw that also accepts tools... (written about elsewhere)
(defrw (mix-old thing-to-mix thing-to-mix-with) ("mix thing-to-mix with thing-to-mix-with"))

(mix 'water 'spork) ; -> '(mix water spork) but should '(mix water) this should NOT be hidden
; in the defintion of mix, it completely defeats reusability, and the correct return is actually
; '(mix water) 'spork, but that is a harder issue :/
; ALTERNATELY WHEEE
(define (using noun verb)
  "use noun to verb"
  "verb must be a procedure"
  ; do we want to do anything about attaching/checking if noun can verb?
  ; or somehow annotating verb to show it is a modified verb?
  ; I think it is more important that using constructions account for
  ; NON CONSUMED INPUTS!
  ; but how would/can we use that to build dependency graphs?
  ; I guess we can treat imports of beings as imlicit imports of a 'procure-being'
  ; or 'make-being' function that is not specified here but could be, and it should
  ; be possible to find those functions from the beings using identifiers
   verb)
(defrw (mix thing-to-mix) ("Mix thing. Extend by modifying mix with using."))
; 'mix' is a perfect example of an underspecified function, there is no specification of how long to mix, nor what the criteria would be to determine whether post conditions on mix have been met (contracts?)
; futhermore it raises an important question about whether names as generic as mix are in fact useful outside of use for local quick references
; a 'mixing protocol' could be tagged as such but beyond a VERY cursory writing of a protocol
; it is not useful

; a related issues is the scope of the executor
; if I have a protocol that is about whisking do I say "hold whisk in dominant hand" etc?
; or do I assume they already know the meaning of (whisk thing)?
; the best example being (snibble thing) which is NOT know and can be taught
; OR can be specified using verbs and nouns that are known
(defrw (cut-1 thing) ("NOT ON THE COUNTER TOP YOU IDIOT"))
(defrw (snibble thing) ((using 'knife cut-into-quarter-inch-cubes) thing))
; how to decompose cut-into-quarter-inch-cubes into a verb and a contract is
; a deeper issue

(defrw (cut-on-cutting-board-1 thing) ("Much better"))
(define cut-2 cut-on-cutting-board-1)  ; better for coping with bad names BUT BAD for documenting I/O, a tool is in the name ffs

(defrw (cut-on-cutting-board-2 thing) ((using 'cutting-board 'knife cut-1) thing))  ; BAD
;(defrw (cut thing) (cut thing)) ; ALREADY BAD, TOO ABSTRACT
(defrw (place thing location) ("move the thing to the location"))

(defrw (hold thing-to-hold-with thing-to-hold) ("hold seems simple enough"))
(defrw (position-touching thing-1 thing-2) ("p-t seems simple enough"))
(defrw (move direction thing-to-move) ("move seems simple enough"))

(define human 'human)
(executor human)  ; do it like the type declarations?
(defrw (cut-4 thing implement surface) (
	  (position-touching
	  (hold 'dominant-hand 'implement)
	  (hold 'off-hand (place 'thing 'surface)))
	  (move 'down 'dominant-hand)))  ; aaand we're back to imperative!
	; ARGH order switching of what object actually makes it out of the call! :/
	;(clean surface)
       	;(clean implement)

; this exercise suggest that we really do need to have executor defined namespaces
; where the criteria is that the executor has all the parts specified in the executor namespace
; when an executor is specified the names in that namespace will be recognized within the scope of that definition
(executor-namespace human '(dominant-hand
			     off-hand
			     brain
			     arm
			     index-finger
			     thumb
			     eye
			     teeth))
       

((using 'spork mix) 'water)  ; this is... execution time stuff? but could be put in a closure
((using 'spork mix) '(water salt))  ; FIXME how to allow rest usage?
(define saltwater ((using 'spork mix) '(water salt)))  ; TODO define shorthand for eval time?
;(defrw (add number1 . number2) ("math!"))  ; FIXME example of fail
(defrw (add number1 number2) ("math!"))
((using 'brain add) 1 2)

(define realworld->number (lambda (subset-of-universe) 'number))
(define measure (lambda (subset-of-universe) 'number))

(def-realworld->digital (measure 'being) ('data))  ; 'threading' magic needs to happen here

; a little != implementation why it is not default we will never know
(define (!= a b . rest-id)
  (if (pair? rest-id)
    (not (apply = (cons a (cons b rest-id))))
    (not (= a b))))

; testing out apply-all
(define (apply-all list-of-functions being)
  (foldl (lambda (function result) (function result))
	 being list-of-functions))

(define fun1 (lambda (being) (+ being being)))
(define fun2 (lambda (being) (* being being)))
(define fun3 (lambda (being) (+ being 1)))
(define fun4 (lambda (being) (/ being 2)))

(apply-all (list fun1 fun2 fun3 fun4) 2)

; yogurt example
#| ; AND THIS FRIENDS IS WHY INCONSISTENT define vs let is EVIL
(define-syntax-rule (repeat-until condition body) ; doesn't quite work
  (define (recurse)
    (if (eval condition)
      #t
      ;(progn ; yay time for languages
      (begin
        (eval body)  ; hidden state, want to get rid of this
        (recurse))))
  (recurse))
|#

(define-syntax-rule (repeat-until condition body)
  (letrec ([recurse (lambda () (if (eval condition)
                                 #t
                                 (begin
                                   (eval body)
                                   (recurse))))])
    (recurse)))

(define (units value unit)
  ; WELL THAT WAS EASY (require measures-with-dimensions)
  ;     except not really because tooling is incomplete :/
  (cons unit value)) ; switch it up just to confuse everyone since putting types first in cons is fun!

(define (has-indentation thing)  ; needs reference points for rigior
  "There is a point on the surface of thing that is
  significantly lower than the surrounding surface"
  (define threshold (units 4 'cm))
  (if (>= (- (delegate-local 'get-surface-max 0 thing)
             (delegate-local 'get-surface-min 5 thing))
          threshold)
    #t
    #f))

(define (press-a-with-b a b) ; man it is hard to define this stuff
  "Apply force on to a using b to mediate the force transfer. Ah glorious delegation :/"
  (delegate 'press a b))

(define (return value) ; TODO
  ; return a value from a monad, which basically means put the name in the local scope
  ; FIXME need to check on the vocab here because i think im brainfarting from haskell
  value)

(define (bind name value)
  ; TODO go read haskell stuff again and sort this out...
  ; naming in this context should put stuff in _some_ namespace
  ; since we named it, even if not the global namespace
  value)

(define (make-indentation-in-a-with-b a b)
  (repeat-until (has-indentation b)
                (press-a-with-b a b))
  (return (bind 'indentation a)))

(define (put-a-in-b a b)  ; need a way to specify executor semantics scope
  "self explanatory given executor semantics"
  ;add a to the contents of b
  (return b))

; alists and lists for sets and stuff... eventually this will collapse under its own weight...
(define definitions-alist '())
(define digital-representations-alist '())
(define acquistion-protocol-alist '())
(define creation-protocol-alist '())
(define thing-list '()) ; keep track of all the things!

(define (validate key thing)
  "if the key exists in the definitions table check it with
  the function at that key"
  (define asdf (assoc key definitions-alist))
  (if asdf
    ((cddr asdf) thing)
    #f))

(define (add-definition name measurements-list validation-function-on-measurements)
  "adds a set of symbolic constraints on the outputs of defined measurements"
  ; we should also enable these definitions to occur in place if possible...
  ; note: as implemented this is last-one-wins if you use assoc
  (cons (list name measurements-list validation-function-on-measurements) definitions-alist))

(define-syntax (ordered-sequence-implicit-output-passing stx)
  (syntax-parse stx
    [(_ x ...) #'(begin x ...)]))

(define (remove-quantity-a-of-contents-from-b a b)
  ; see also take-a-from-b TODO, dont need this now, but may come back to it...
  (if (has-digital-representation? b)
    (remove a b) ; h-d-r useful to implement digital accounting
    (delegate 'remove a b)) ; we have to delegate tracking to the executor
  )

(define (*contents* thing)
  "This is essentially an accessor function that
  provides direct access stuff inside. It will have a
  lisp function equivalent 'contents that can access the
  black-box and/or bbc1 to obtain a actual lisp list of their
  contents when other real-world functions *put* stuff in them"
  ; TODO use contents to get back any symbolic representation of
  ; the 'known' contents of thing
  '())

(define (*generic-unpack* container-thing) ; combat conflation of containers with their contents
  "Did your thing come in a container?
  If so, please remove what you actually wanted from the
  container containing it that you conflated it with! :)"
  ;(define quantity 'how-should-we-deal-with-free-variables-in-real-world-functions?)
  ;(remove-quantity-a-of-contents-from-b quantity (*contents* container-thing))) ; WRONG
  (*contents* container-thing)) ; this is really what this is...
(define (mix-a-using-b a b)
  ; mix-a-with-b was confusing because with is ambiguous and could imply combining a and b
  (delegate-local 'mix a))

(define (carefully verb)
  "ah adverbs! what ever shall we do with you" ; for now nothing!
  verb)

(define (mix-carefully-a-using-b a b)
  ; TODO another example of a useful higher order function...
  ; higher order real world functions don't actually work though...
  ; because the full process encapsulated in 'mix cannot be shoved in as
  ; an input to 'carefully, aka, wth are adverbs?!
  ; adverbs are either directives or posterior judgements (measurements) about
  ; a process... they are not actually higher order functions because they
  ; are purely symbolic and cannot take the partial black-box implied by a real-world
  ; function as an input... or... maybe it can? more though required here...
  ((carefully mix-a-using-b) a b))

(define (*make-delicious-yogurt* yogurt honey granola blueberries bowl spoon)
  ; observe the implicit conflation of yogurt with yogurt container... this is a VERY common pattern
  ; the major issue here is that the conceptual item that we want is part of our _output_
  ; is NOT the thing that the executor can act on in order to make the output (in this example the containers
  ; of yogurt, honey, granola, and blueberries) this pattern is something we need an easy way to combat
  ; since we aren't trying to do logic programming... /me imagines free-floating yogurt flying about
  (validate 'yogurt yogurt)
  (return (bind 'delicious-yogurt
    (ordered-sequence-implicit-output-passing ; progn
      (put-a-in-b yogurt bowl) ; put is vastly under defined (put-a-in-b-using-c), as is yogurt :/
      (let ([indentation (make-indentation-in-a-with-b yogurt spoon)])
        ; nested lets using a macro don't quite work because we dont know
        ; where the close paren needs to go, but doing it manually isnt the worst... maybe a rename?
      (put-a-in-b honey indentation))
      (mix-a-using-b (*contents* bowl) spoon) ; fun thought here about beating eggs since air is never listed...
      (put-a-in-b blueberries bowl)
      (mix-carefully-a-using-b (*contents* bowl) spoon)))))

; som protocol from mlab, variants

(define (prot-name-func)
  (define prot-name-start 100)
  (lambda () (begin0
               (format "protc-local:~a" prot-name-start)
               (set! prot-name-start (add1 prot-name-start)))))

(define get-default-protocol-name (prot-name-func))
(define (protocol #:name (name (get-default-protocol-name)) inputs outputs)
  "MAJOR TODO since protocols are sort of supposed to be whole files")
(define (inputs . rest) (list rest))
(define (outputs . rest) (list rest))
(define (real-world-assembly . rest)
  (flatten rest))
(define (instance thing type)
  ; TODO syntaxxxxxx
  ; ya... need to actually do something about this...
  thing)

(protocol #:name 'som-protocol
  (inputs ; opening with inputs may not always be how people write these... sometimes they should/may start with a single step
	'male-mouse-strain-gin
	'female-mouse-white ; no memory of the actual name
	'patch-pipette
	'needle-beveler
	'...  ; as we can see this method is BAD for composability
	)
  (outputs
   'dead-mouse-parts
   'fixed-brain-slices ; separation of concerns passing...
   'acsf))

(define headstage (real-world-assembly
					'headstage-electronics-box
					'electrode-holder-gasket
					'electrode-holder-base
					'electrode-holder-oring
					'electrode-holder-screw-clamp
					'electrode-wire))

(define rig (real-world-assembly
			  'computer
			  'acquisition-amplifier
			  'patch-amplifier
			  'headstage-2 ; note that this is a perfect example for 'elaboration'
			  (instance 'headstage-2 headstage) ; elaboration only requires wrapping
			  'halogen-lamp
			  'filter-set
			  'objective
			  'microscope-body
			  'linear-actuator-1
			  'linear-actuator-2
			  'led-470nm ; embedded parameter limited by the channelrhodopsin...
			  ))

(define (add-causal-link-a->b a b)
  ; todo state ;_;
  (cons a b))

(define (parameter-coupling target-setting source-number)
  ; observe that source-number can be the output of a measurement
  ; single time vs repeated measurments... how frequently do I need to check this
  ; there is a trade off here: we don't just want the output of what evaluates
  ;     to source-number, we want the structure itself... might need to modify the reader?
  ;     aka this probably needs to be a macro or something...
  ;     the 'normal' evaluation strategy from lisp isn't quite right for our use case...
  (add-causal-link-a->b source-number target-setting))

(define (make-executor description . verb-lists)
  ; just alist for now, because why not use dicts for everything
  (list (cons 'description description)
        (cons 'verbs (flatten verb-lists))))

(define (known-functions executor)
  ; aka known-verbs
  (cdr (assoc 'verbs executor)))

(define english-speaking-executor
  (make-executor
    "anybody who speaks english"
    ; closed world vs open world here, the need to use a dictionary is problematic...
    'remove
    'put
    'lift
    'shake))

(define biologist-executor
  (make-executor
    "This is someone who has had enough training
    in biology to understand the words used here
    and yes, this definition is circular."
    (known-functions english-speaking-executor) ; may need to unpack this?
    'pipette))

(define (bind-delegate executor)
  (lambda (function-name . rest)
    (if (member function-name (known-functions executor))
      #t ; because executor functions are delegated, they do not interact with the digital world directly, their digital implementation should be handled elsewhere... need to think about how to bind these...
      (error "executor does not know that function..."))))

(define (delegate-local function-name return-value-or-generator . rest-real-world)
  ; sometimes we want to delegate on the fly... how can we support this...
  ; this should probably add function-name to the currently scoped executor?
  ;     delegate-local skips the validation step, and in some sense these should be added and flagged
  ;     as forms that -may- almost certainly need further elaboration
  return-value-or-generator) ; ON THE OTHER HAND... it is extremely useful for delegated functions to return values...

(define delegate (bind-delegate biologist-executor))

(define (has-digital-representation? thing)
  ; TODO type vs namespace issue again
  ; set membership seems so much... easier...
  (define maybe (assoc thing digital-representations-alist))
  (if maybe
    (cdr maybe)
    #f))

(define (take-a-from-b a b) ; observe that as written this does not work...
  (if (has-digital-representation? b)
    (remove a b) ; h-d-r useful to implement digital accounting
    (delegate 'remove a b)) ; we have to delegate tracking to the executor
  a)

(define (*contains-a-inside-b* a b) ; TODO
  (if (has-digital-representation? b)
    (member a b)
    (member a (*contents* b)))) ; problem ;_;

(define (has-creation-protocol? thing)
  ; ok, this is weird, because we don't actually want (*has-creation-protocol? thing)
  ;   in this case we are treating 'thing as the digital proxy for the real-world noun
  ;   I think we are going to need a way to clarify this, using the lisp function version
  ;   is actually a pretty nice way to capture the shared semantics of the symbol for a thing
  ;   dereferencing is another option, though obviously being able to (dereference-symbol 'tiger)
  ;   might have... undesirable consequences for your cpu and the universe at large
  (define maybe (assoc thing creation-protocol-alist))
  (if maybe ;(member thing thing-list)
      (cdr maybe)
      #f))

(define (has-acquisition-protocol? thing)
  (define maybe (assoc thing acquistion-protocol-alist))
  (if maybe
      (cdr maybe)
      #f))

(define (*cry*) "yep")

(define (retrieve thing)
  (implicit known-storage-location)
  (cond [(*contains-a-inside-b* thing known-storage-location) (take-a-from-b thing known-storage-location)]
        [(has-creation-protocol? thing) ((has-creation-protocol? thing))]
        [(has-acquisition-protocol? thing) ((has-acquisition-protocol? thing))]
        [#t (*cry*)]))

(define-syntax (steps stx)  ; TODO ...
  (syntax-parse stx
    [(_ x ...) #'(list x ...)]))

(define (step name inputs outputs) (list name inputs outputs))

(steps
  (step 'make-acsf ; observe that reference to acsf should resolve to this if it is not in the fridge...
        (inputs '2L-beaker
                '2L-volumetric-flask
                'DDI-water
                'NaCl
                'KCl
                'd-glucose
                '(dot MgSO4 7H2O)
                'sucrose
                '(dot CaCl2 2H2O)
                'NaHCO3)
        (outputs 'acsf))
  (step 'slice-setup)
  (step 'make-slice
		(inputs 'mouse ; problem: quoting makes dupe checking hard
				'vibratome
				'disection-tools
				'glue
				'agarose 
				'KX
				'cutting-buffer
				'perfusion-pump
				'25-gauge-needle)
		(outputs '500um-brain-slice)))

(define (black-box-complement-1 #:name name . rest) ; MASSIVE TODO
  "container for names of things asserted to not be causal
   in the dynamics of the measurement in question...")

(define (black-box #:name name . rest)
  "building block of all measurements, asserted to contain the cause of measurements")

(define to-measure-1
  (black-box-complement-1 ; implied bbc2
	rig ; it seems like we need to be able to pack many details behind a name?
	(black-box #:name 'bb-patched-pair
			 ; at specification time names should link entities? shouldn't have to pass in at 'runtime'?
			 'cell-1 ; this is the product of a massive set of transformations...
			 'cell-2
			 'brain-slice ; where do we draw the boundary here? shouldn't this be acounted for as part of the selection of cell-1 and cell-2????
			 'bath-solution ; writing down successive black boxes that express invariants at various levels of granularity might be helpful... but how to do it? even though black boxes are the inputs of measurements they are also the output of the series of transformations
			 )))

; start from list of reagents, start from a step, start from a tool description, start from a measure, start from a black box, start from a black box complement, start from a set of parameters
; transition from ill-defined in-place atoms to externally defined constructs
; WHILE allow for non-local reference to parts... is it reasonable to allow for
; non-local references, so that everything gets dumped into the same namespace?
; you're gonna get sick of rig->headstage-1->electrode
; similarly you are going to get sick of specifying nested black boxes that are redundant
'(black-box-tree)

; an approach that allows starting from lists of globally known inputs for the more list-centric among us...
'(global-inputs)
'(global-intermediates)
'(global-outputs)
'(global-measures)

; criteria for transformation vs simply collections is mediated by the reversibility of the process
;     so basically as long as delta s doesn't increase (eg a mixture) we are ok?
; use of ! for signaling modification... ala set!, the language for real-world objects could get confused
;     also (press-a-with-b 'yogurt 'spoon) is ! while (press-a-with-b 'steel-plate 'spoon) is probably more 
;     likely to result in a substantial modification of the 'spoon instead of the 'steel-plate...
;     furthermore, modification is really an assertion here...
;     perhaps like... ((destructive press-a-with-b) 'dangerous-clay-animal 'hydraulic-press)
;     actually that is not a bad idea... modifying the activity of the function, but need a consistent way to
;     deal with i/o and what argument is destroyed... ((destructive 'a press-a-with-b) 'a 'b)

; mapping between real-world functions and lisp functions
;     having played around with this a bit more it is clear that lisp functions and real-world functions
;     need to be different, namely they need to have different types
;     in addition real-world functions should have some lisp-function that represents it
;     but that lisp function needs to be a derived function
;     the question now is whether real-world functions need to always operate on real-world types as well
;     or whether we can get away with using atoms... I think using atoms may ultimately be confusing...
;     the full explanation for differentiating types is because lisp functions are F: symbol -> symbol
;     whereas real-world functions are F: real-world -> real-world and measurements are F: real-world -> symbol
;     providing some light syntax might help, *real-world-function*, lisp-function, *measurement-function,
;     parameter-function* or something like that... if we need a full on type systems for these we can...

; useful common measures and supporting cast

(define-syntax-rule (implicit parameter)
  #|"implicit defines a hint when generating
   code that defines measurement function
   this is needed to actualize the measurement
   otherwise the number recorded would always be 0"|#
  ; note that this implies that there is some missing
  ; anchor in the real world that is needed that cannot
  ; be 'measured' but must be chosen (see *center-of-mass)
  (define parameter (quote ,parameter)))

; assumptions will be modelled as assertions about the outcomes of protocols that we are not going to execute
;   they need to be specified with as much rigor as possible, but like any other protocol, should be able to
;   function without full specification

(define-syntax-rule (assumption lambda-list assumption-definition)
  (eval assumption-definition)) ; FIXME

(define-syntax-rule (assume assumption-definition body)
  #|"local function for defining an assumption in place"|#
  ; TODO syntaxxxxxx
  (eval body)) ; FIXME

(define-syntax-rule (under assumption-application body) ; (under assumption about)
  #|"apply an existing global assumption type locally
   the target of the assumption needs to be defined"|#
  ; TODO syntaxxxxxx
  ())

(define (get-assumption-protocol assumption)
  "I have no idea how to implement this right now")

(define (*holds? assumption black-box-spec) ; hrm... how can you pass black-boxes to protocols instead of just measurement steps...
  "*holds is a theoritical measure that is parametrized by an assumption
   and returns true over a being-subset/black-box/environment if the measures
   taken on that black-box are consistent with that environment. NOTE that we
   can't actually ever KNOW that *holds? generalizes (thanks Hume) but we still
   need it since assumptions holding on a subset of measures are often critical
   validations that other measurements are not tainted"
  ((get-assumption-protocol assumption) black-box-spec))


(define (*density thing)
  "NOTE THAT THIS IS A CALCULATED MEASURE!")

(define (*all-possible-spatial-divisions* thing)
  "a weird one")

(define (uniform-density thing) ; FIXME define-assumption
  (equal? (map *density (*all-possible-spatial-divisions* thing))))

(define (norm ndpoint1 ndpoint2)
  (cond [(and (number? ndpoint1) (number? ndpoint2))
         (abs (- ndpoint1 ndpoint2))]))
        ;((/= (length ndpoint1) (length ndpoint2))  ; wat
         ;#f)
        ;(#t (sqrt (apply + (map 'list 'square (mapcar - ndpoint1 ndpoint2)))))))

(define (*center-of-mass thing)
  "amusingly enough, aside from guessing and making assumptions
   about uniform density, this is really hard to determine using
   non-destructive techniques"
  ; note that there is an implicit reference frame
  ;   if we want to convert this to an actual symbolic representation
  ;   the brain has it's own reference frame for actions because it
  ;   also maintains a symbolic representation of the external state
  ;   of the world
  ; one way that we could make the "implicit" explicit in real-world functions
  ;   would be to allow for dangling free variables... or... no
  (implicit reference-point)
  (norm reference-point
        (under (uniform-density thing)
               (cond [(and (*convex? thing) (*topo-sphere? thing))
                      (minimize (surface-points thing))]
                     [#t *guess-a-point thing])))
  )

(define (*shake-alt* thing)
  (repeat-until (shaken? thing) ; FIXME repeat-at-frequency-until :/ is there a good way to do this? this isn't declarative... we need a decl version
                (begin (*displace* (*center-of-mass thing)
                                   (units 5 'cm))
                       (*displace (*center-of-mass thing) (units -5 'cm)))))

(define (*shake* thing)
  (repeat-until (shaken? thing)
                ((at-frequency (units 10 'Hz) *displace*) ; one way we could parameterize steps... I think this may be troublesome because it can be hard to elaborate...
                 (*center-of-mass thing) (units 5 'cm))))

; basic short verbs, in this version using *verb* for real world becomes quite annoying...

(define-syntax (*make* thing from how) ; (*make* output inputs body)
  ; production is the term used elsewhere in my writting
  ; return value goes first because english
  ; explanation goes second, we drop the implicit 'by'
  ; the 'from' is the inputs
  '())

(define-syntax (*get* thing from how) ; here from is a location not ingredients... bloody english
  ; acquisition in a _local_ scope, this is more a local restriction, fits with 'must be kept @ 4C'
  '())

(define-syntax (*buy* thing from how) ; XXX do not use, this is a variant of *get*...
  ; woo something that can actually hook into an API and buy directly...
  '())

(define-syntax (*check thing parameters how) ; this is too simplistic... and it is not clear that check is simply a measure, it seems more like a measure + reaction
  '())

(*check 'melon ('(ripe . #t) '(overripe . #f))
        "thump 'melon with your hand to see if it makes a nice hollow sound,
         but frankly you need someone to show you ripe vs unripe,
         and this doesnt always work.
         Overripe will sound too deep? (honeslty I have no idea)")

; how to communicate that you don't need an exact match for some input

(define (assertion-old type value target)
  "Add a logical note that the exact tool/reagent
should not affect the outcome of a step.")

(define-syntax (assertion assertion-statements)
  ; code to check whether assertion statements are valid
  ; code to stick them in the constrains list
  ; need to distinguish between (assert thing) type behavior "assumption"
  ; TODO how provide the ability to refer to data that supports this
  '())
(define-syntax (assumption assumption-statements)
  ; I want to assert this but I don't have any data to back it up
  '())

(assertion 'input 'exact-match-unimportant 'disection-tools)
(assertion 'input 'output-should-not-be-affected-by 'dissection-tools)
(assertion (can-vary* #t dissection-tools))
(assumption (can-vary* #t dissection-tools))
(assumption (can-vary* #f RRID:AB_123456))

; updated verions of make-delicious-yogurt
; initial observations here seem to suggest that *make*, *get*, *check steps need to be inside a larger structure for steps
; they are behaving like anonymous functions in a sense, we haven't provided a simple way to name them... eg #:name ??
(*make* delicious-yogurt (list yogurt honey granola blueberries spoon bowl) ; the keyword makes this more readable? #:inputs
        (map (lambda (input) (*check (*get* input))) inputs)
        ; vs
        (map *get* inputs) ; implicit state here :(
        (map *check inputs) ; implicit state here as well... (which doesn't actually work)
        ; vs
        (set! inputs (map *check (map *get* inputs))) ; observe that *check and *get* are being treated like generics here...
        ; consider a case where *check fails... what do we do? moreover, if check fails that may be fine, we could go to the store
        ; but what if going to the store would put us outside the time restrictions on our experiment!
        ; having a setup checkpoint might be a better way to proceed (HRM!)
        
        (ordered-sequence-implicit-output-passing ; progn
         (put-a-in-b yogurt bowl) ; put is vastly under defined (put-a-in-b-using-c), as is yogurt :/
         (let ([indentation (make-indentation-in-a-with-b yogurt spoon)])
           ; nested lets using a macro don't quite work because we dont know
           ; where the close paren needs to go, but doing it manually isnt the worst... maybe a rename?
           (put-a-in-b honey indentation))
         (mix-a-using-b (*contents* bowl) spoon) ; fun thought here about beating eggs since air is never listed...
         (put-a-in-b blueberries bowl)
         (mix-carefully-a-using-b (*contents* bowl) spoon)))

; how to bind these? does an object system make sense here?! :( because that means too much specification
; running a linting pass to warn that no check has been defined for '(noun . honey)...
(*get* honey kitchen-counter
       (*grab* honey))
(*get* honey (lookup-location honey)
       (if location
           (*grab* honey)
           (*purchase* honey (lookup-vendor honey)))

(*check honey ('(expired . #f))
        "Honey doesn't expire. There are reports of perfectly good
         (if crystallized) honey found in ancient burrials!")

(define-syntax (number-output-spec direct-or-computed type-of-number units-or-type-for-count) ; this actually may be the most important to get right...
  ; this defines primative data types, NOT COMPUTED
  ;   but like elsewhere we need to support the ability so specify timeseries with parameters (for example) while also warning that
  ;   using a timeseries as a direct-measure obscures the fact that the record of the black box will remain fixed... (or something like that)
  ;   this reveals that in addition to direct-measure, and computed-measure, we may also want composed-measure which is one in which no values have been
  ;   changed, merely that they have been arranged and that their arrangement has some specific semantics... hrm... the semantics of arrangement... HRM
  '())

(define-syntax (*measure-v1 black-box number-out-spec operations-on-members-of-the-black-box) ; TODO all these syntax definitions don't actually work correctly, but are placeholders
  ; the issue with a pattern like this is that we would like to be able to reference the
  ;   members of the black box by name within this namespace, HOWEVER that means it is no longer
  ;   a black-box... grrrr, in addition there is the associated bbc chain... also measurement can
  ;   happen at any time... so we need to think more clearly about exactly how these should work...
  ;   perhaps we can allow references to names in the black-box as assertions/assumptions and simply
  ;   validate that those names have been put into the blackbox by the point where measure is actually called
  ;   what this means is that measure does not take black-box as an argument at definition time, but at call time
  '())

(define-syntax (*measure #:name name output-spec acquistion-code body)
  ; restrictions on black-boxs
  ;   1) any reference to a name should be in the black-box at call time
  ;   2) the actual digitization equipment (ADC or a pencil or pen) needs to be in bb or bbc (support for implicit probably useful)
  ;   3) additional restrictions shall not be included in measurement steps, logically nothing can happen before symbolization
  ;   4) measurement steps may document the perceived set of transformations on the being-subset of interest that mediate symbolization
  ; should the implementation of the acquistion go here? or should a reference to it go here? I think we need to support both...
  ;   this can allow the output spec and the acquistion-implementation to sit side by side if needs be...
  ;   in most cases this will probably be async code to collect results from some data producer...
  ; like with all functions, definition and calling need to be separate, if a name is defined we need to bind the output function to that name
  ;   in the local scope OR if not, then the measure will be treated as a lambda that can be used in-place on a black box, this is nice because
  ;   it allows for the 'promotion' of measures from local one-time to reused, even though the inversion of temporal order might confuse
  ;   we can build a syntax transformer that can convert a linear step-sequence into a series of function applications going the other way
  ; mediation for things like *count where a human brain is in the loop... we don't have a good implementation there, but that can go in the bbc...
  ;   measures can also use other measures... internally so (*measure #:name *count-ducks ...)'s body looks like (*count (*identify-duck-like-shape (*scan-environment 'local-environment "produces vision data")))
  ;   or something like that, though in reality it might make sense to treat these as asynchronous generators... since that would be a much cleaner
  ;   way of communicating about it... though there is a risk the protocol would never stop... in imperative it goes while duck-not-found; if not pretty-sure-you've-looked-everywhere; look-for duck
  ;   interestingly the implementation of count here looks like it might be incrementing when the interior function returns true... HRM how can we make measurements chain
  ;   FALSE: measurements DO NOT chain. the correct version of this is (count (*identify-thing-to-count (*visually-scan-environment* space-you-are-in)))
  ; if there is a name it should fit the *name syntax (for now)
  (when in-mode-acquistion (acquistion-code)) ; this needs to go in the generated code...
  (lambda (black-box) (body)))

(define acsf-osmolarity
 ((*measure ('direct 'decimal-4sigfigs 'osmolarity)
            (read) ; we may need another macro to make this work... otherwise the body may try to eat it
            (*read-number-off-device (*push-button* omolarity-device)))
  ; TODO there is a big gap here, I have NO idea what the osmolarity device is actually measuring to produce its number, nor its principles of opperation :/
  ;   the very nice issues that this raises is how to grow the protocol for cases like this, and where it needs to go
  ;   because it whoooooole bunch of stuff is hidden behind *read-number-off-device and *push-button*, so we need a way to expand these definitions locally here...
  (*black-box-hierarchy*
   (*make* acsf-sample-in-osmolarity-device ; should this be *make*? or should this be *black-box*?
           (list
            osmolarity-device
            (*make* acsf)) ; observer here that there is an implicit dependency chain which we would want to make accessible explicity
           ; fun observation about (*make* thing) is that we _could_ try to automatically expand *make* syntax that only had a single name to more extensive definitions elsewhere (ooooo)!
           (assumption (homogenous? #t acsf)))
   (list room-temperature elevation air-pressure dust-particles-per-m^3) ; this is fun because you can just stick in your classic list of potential confounds right here!
   (list things-we-know-nothing-about)))) ; HRM how do we talk about pushing buttons on devices? *do*???

(define (*black-box-hierarchy* black-box black-box-complement-1 black-box-complement-2 . rest) ; should probably be syntax or something...
  "an endless list of black box complements. the fun part is that if we do this right
we should be able to trace everything back that we have explicit prov for :D")
