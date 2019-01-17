;;; core evaluation model
#lang racket/base

(require (for-syntax racket/base syntax/parse racket/syntax)
         racket/generic
         racket/bytes
         syntax/srcloc
         )
(module+ test
  (require rackunit))

;;; functions
;; black-box
;;   proper-name
;;   categorical-name
;;   names can be either
;;     singular aka token
;;     composite aka collection

;; agent (is a properly named black box with some additional properties)
;;   executor an agent that can understand an execute this language
;; agreement group (collection of agents)

;; aspect
;;   correctness of binding of aspects is another issue
;;   in a purely physicalist ontology is it possible to define aspects
;;   using only black boxes without the need for aspects?
;;   yes, I think this is purpose that particles serve ...
;;   HOWEVER knowledge those particles are secondary in time
;;   so the question is how to flag that an aspect is based on
;;   some convention such as the mass of a proper name
;;   the objective is to define aspects based soely on categorical names
;;   however that is usually not where we start
;; dimension (scalars)
;;   ranking
;; contextual (vectors or similar) dimentions e.g. orientation ; ? is this a dimension? not really ...
;;   just like you can rank scalars you can sort of rank, or at least check equiality on vectors
;;   the context vectors must match (as closely as possible ...)
;;   you can think about failures of generealization as mismatched orientations in high-d contextual space
;;   projecting onto a seeminly similar 'dimension' when the orientation is not matched will produce
;;   an incorrect result
;;   the coordinate system in which these 'vectors' are defined does not have to be the real numbers
;;   quite related to machine learning ...
;; unit

(module+ test
  (define-black-box )
  ; processs for verifying that an agent has a ruler
  ; given the dpi of the attached screen
  ; render a line of random known lenght and ask them
  ; to enter their result in centimeters
  )

#;
(define-syntax (define-context stx)
  (syntax-parse stx
    [(_ thing)
     #'()]
    )
  )
#;
(define (context thing)
  (vector-aspects . (location))
  (syntax-parse stx
    [(_ thing)
     #'()]
    )
  )
#;
(define (make-context))

;;; primer on proper names
#<<--rambling--

Conceptually this should construct a function that when given some black box
returns true iff the black box is identical to the black box originally assigned
the name in actuality returns a symbolic function that acts as the identity predicate
for that black box. The actual implementation of such an identity function can rely
only on measurements on the context, it cannot rely on measurements of the phenomenon
itself. Actual identity functions may also be probabilistic.

amusingly this is also where cogito comes in again, because individual agents
can only attest that they themselves are the only thing that can be properly named
with probability 1. Some might argue that even that is tenuous, but I would argue
that that property is in fact definitional, and an agent that cannot attest to its
own proper name cannot be the same agent (i.e. that if the scratchings 1 attests that
it is in fact the scratchings 2 then it existential-is 2)

not going to do this with multiple dispatch
a symbolic representation of the context (aka black-box-complement)
is passed in instead and the body can use whatever concepts are prior
in time to define the predicate

validate context could be mutually recursive? NO
contexts cannot include directly or transitively in them any reference
to the proper name that they are used to define
annoyingly the absense of a privilidged place means that at some point
our naming has to either find a stable mutually recursive definition
or it must include time in some way, e.g. the pusar position system
e.g. using the 3d positions and spectra of nearby stars it would be
possible to construct a completely scalar definition of Sol, but
the proper name function of those stars could not rely (as they do
traditionally on their location in the sky, since that depends on
earth's location which is the aspect of a proper name and thus

--rambling--

;; naming
;; these are essentially predicates that take a symbolic name + context and expand it to
;; a function on the symbolization that must be provided for that name
;; if we were to use types then the type of the first argument should be considered
;; to be equivalent to a black box that has the name etched on it even if the
;; the context is literally the set of all other names that point directly
;; to a naming function in the current context
;; functions on being names (ya types help here) must specify the aspect
;; that is being measured and that aspect must be defined in a way that is
;; independent of the name in question (woo circularity)
;; NOTE also need an error function which allows equivalence under a definable erf
;; we exploit lisp lexical scoping as a proxy for contexts
;; requiring contexts is equivalent to expanding the context
;; the reason why we need implementation decouple (at the module level)
;; is so that we can refer to a name which may have multiple definitions
;; [single counter example sufficient to prove naming functions disagree]
;; and we use a "because I trust the vendor" or "it was labeled as such"
;; criteria instead of "this is a fundamental scientific definition"
;; we need this so that free variables do not have to be explicitly lifted
;; into an outer containing function -- however, we do need types to make it work
;; this should really be implemented in turnstile if we are using types
;; but for now we are going to do it with lists

(define-for-syntax type-being-list '())
(define-for-syntax (add-being identifier)
  ; use the symbol 'i-am-a-smbol rather than the syntax
  (set! type-being-list (cons (syntax-e identifier) type-being-list)))

; name is the local name for protc, it can be globalized in a number of different ways
; use pipe syntax to include spaces in names |this is a name with spaces| 
; you may have more than one name-function in a single module and when
; augmented with a result storage backend or a simulation function you
; can immediately alert if some instance causes the definitions to be incompatible
; if more than one function is defined then only functions where all free variables
; are defined in the current-context at runtime will be evaluated in untested will
; be marked. the empty naming function is allowed and default behavior is to use
; everything? as the naming function, though using nothing? is probably better
; so as to alert people to the issue, probably need a way to continue execution
; in the nothing case ...
(define-syntax (define-name stx)
  (syntax-parse stx
    #:literals (define)
    [(_ name:id (define (naming-function:id aspect:id ...) body:expr ...) ...)
     #:do ((add-being #'name))
     #:with predicate? (format-id #'name
                                 #:source #'name
                                 "~a?"
                                 (syntax-e #'name))
     #:with ((aspect-value ...) ...) (map (λ (a)
                                            ;(println a)
                                            (format-id a
                                                  #:source a
                                                  "~a-value"
                                                  (syntax-e a)))
                                          (syntax->list #'(aspect ... ...)))
     #'(begin
         (define (naming-function aspect ...) body ...) ...
         (define (predicate? thing)
           ; this way of defining a predicate is NOT indended for
           ; rendering as a protocol, because there is no ordering
           ; nor anything else, need to deal with that elsewhere
           ; when I ask predicate? during definition measure needs
           ; to return a representation for export and runtime
           (define aspect-value (measure thing aspect)) ... ...
           (and (naming-function aspect-value ...) ...)))]))

(module+ test
  (define (contains? thing value) #f)
  (define-name thing)
  (define-name thing2 (define (nf a b) #f) (define (nf2 c d) #f))  ; wtf
  (define-name thing2 (define (nf)))
  (define-name mouse
    (define (m1 rna-seq-mammal-16s-equivalent)
      (contains? rna-seql-mammal-16s-equivalent "i am a mouse")))
  (mouse? "hohoh")
  )

(define universal-context (context '((category . '(member-1 member-2))) (execution (make-execution-id))))

(define (bound-in-context? thing context)
  ""
  ; this is a measurement function which must actually be true whether or
  )

(define (any lst)
  (for/or ([l lst]) l))

(define (already-bound? thing)
  ; not in any known alist TODO
  #f)

(define (make-proper-name categorical-name [current-context universal-context])
  (λ (thing)
    (and (categorical-name thing)
         ; not already named <- this shit is hard
         (not (bound-in-context? thing current-context))
         (not (any (map (λ (proper-name) (already-bound? thing)) (proper-names current-context))))
         (context ))))

#;
(define-syntax (proper-name stx)
  (syntax-parse stx
    [(_ name:id context-definition:sc-context ...)
     #'(define (name context)
         ( )
         )

     ]
    )
  )

(module+ test
  (define (anything black-box black-box-complement)
    "Is this anything? Yep!"
    #t)
  (check-true (anything "asdf" null))

  (define (existence? [black-box] [black-box-complement])
    "accepts on anything including the null set but it cannot distinguish between
     arguments that cannot be expressed symbolically and reduces all of them to the
     empty set"
    #t)
  (check-true (existence?))
  (check-true (existence? null null))
  (check-true (existence? "thing" "context"))

  (define (my-name black-box black-box-complement)
    ; categorical criteria
    ; literally has a symbol attached to it criteria
    ; by other proper names criteria
    ; "make michigan is body of water that looks like <image>" ... hrm?
    (define (meets-categorical-criteria? black-box) )
    (define )
    (meets-criterial? black-box)
    (define context-0
      (()
       )
      )
    (context-match? context)
    )
  )

(define-syntax (categorical-name stx)
  "criteria"
  (syntax-parse stx
    [(_ (thing) more)
     #'"we catually make something"]))

(define-syntax (cat-name stx)
  (syntax-parse stx
    [(_ inner-stx)
     #'(categorical-name inner-stx)]))

(define (rank-dimension dimension . black-boxs)
  ; for each
  ;(let* ([black-box-aspect-pair (car black-box-aspect-pairs)])
  (let* ([black-box-aspect-pair '(lololololol)])
    "TODO"
    ))
(define (read-bound-symbol black-box)
  "If there is a symbol on the black box then you can read it."
  )
(define (find-bound-symbol black-box)
  ""
  )
(define (count-categorical black-box-component)
  ""
  )

;; execution
; TODO consistent rules for constructing names
; FIXME horrible design here
(define (get-current-executor)
  "This is overseer stuff ... esp due to possibility for multiple users"
  ; TODO
  "unknown"
  )
(define (make-execution-id)
  ; FIXME horrible way to do these, maybe uuid better, maybe not
  (define hrm (format "~a|~a|~a"
                      (current-milliseconds)
                      (source-location-source #'woo)
                      (get-current-executor)))
  (println hrm)
  (sha256-bytes (string->bytes/utf-8 hrm)
   ))

(struct execution (proper-name) #:inspector (make-inspector))

;; contexts
(define-generics gen-context
  (proper-names gen-context)
  (categorical-names gen-context))
(struct context (category-alist execution) #:inspector (make-inspector)
  #:methods gen:gen-context
  ; TODO
  [(define (proper-names context) '(thing1 thing2 thing3 ...))
   (define (categorical-names context) #hash((cat1 . '(thing thing2 ...))
                                             (cat2 . '(thing3 ...))))
   ])

(define (categorical->proper categorical-name)
  ; TODO FIXME want to use module scope + name type for this ...
  "obtain the proper name of the instance of a categorical name in the current context
   fails if there is more than one matching proper name in the current context
   this is a type -> token or type -> instance lookup"
  (parameterize ([current-context (get-current-context)])
    (let ([members (hash-ref (categorical-names current-context) categorical-name)])
      (if (> 1 (length members))
          (error "U WOT M8")
          (car members)))))

(define (categorical->many-proper categorical-name context)
  "get all proper names in the current context that correspond to a categorical name
   NOTE: need to implement cat->super-cat")

;; aspects

(define (aspect->dimension lone-aspect)
  ; TODO do dimensional analysis via rosette
  1)

(define (actualizable? aspect)
  ; TODO
  (aspect->dimension aspect))

(define (categorical? name)
  "FXIME not even close to right"
  #t)

(define (proper? name)
  "FXIME not even close to right"
  #t)

(define (proper->categorical name)
  "get the category of a peroper name aka go up a level"
  ; TODO
  'lol)

(define (vector-aspect? thing)
  ; fixme clearly wrong
  (list? thing))

(define (scalarizable? aspect name)
  "do we know how to uniquely scalarize this aspect for this name"
  (let ([working-name (cond [(categorical? name) name]
                            [(proper? name) (proper->categorical name)]
                            [else (error "what bloody kind of name have you given us?!")])])
    ; this is one of those places where we need to try to help people as much as possible
    ; by informing them of missing information
    ; this is orthogonal to actualizable? so for example mM cannot be scalarized
    ; because we are missing a value, either grams or volume that we need to solve for
    ; concentration is scalar because there is no information missing, it is the invariant
    ; on the other hand (: length) object with dimension greater then 1 cannot be
    ; instead we need (:: spatial-1 projection) where projection is a function
    ; that tells us how to eliminate the other dimensions in a reliable way
    ; spatial-2 is area or surface area, and spatial-3 is volume
    ; the basica properties that we need to know for any name is whether
    ; it retains its name under deformation, otherwise we assume a rigid body
    ; this allows us to deal with things like the length of a blob of silly putty
    ; scaling is impossible due to conservation of mass, and translation is
    ; irrelevant since the interal frame for a context free aspect always follows
    ; the object in question, therefore the only thing that we need to worry about
    ; is rotation of a rigid body, or viewed another way the various symmetries that
    ; the object has are the primary problem for naming, so we need to be provided
    ; with as many constraints as the object has symmetries
    ; e.g. for a piece of paper you need 3, for a sphere you need 4, for an iregular
    ; triangle you need 0 because there are no symmetries and you can use ranked side
    ; lengths
    ; https://en.wikipedia.org/wiki/List_of_finite_spherical_symmetry_groups spheres require 6???
    (if (vector-aspect? aspect)
        'yay!
        'nooooooooooooooo!)))

(define (aspect->unit aspect)
  "Given an aspect returns the unitization from the preferred system of units.
   If the aspect is a vector then the result will be a vector unit and the first
   aspect is what will be unitized"
  ; FIXME I think all components of a vector need to be unitized no?
  (define unit-for-aspect 'TODO)
  unit-for-aspect)

;; projections
(define (project-unit->categorical-name unit proper-name)
  ; TODO check whether we need to define a function
  ; that transforms a vector aspect from an enclosing
  ; black box to a part, so that things like orientation
  ; can be preserved, or whether aspect specs like this
  ; carry the information about the thing that they were
  ; originally bound to (I don't think they do?)
  (categorical-name (thing)
                    (and (part-of? thing proper-name)
                         (= (measure thing unit) 1))))

(define (project-aspect->categorical-name aspect proper-name)
  "Given an aspect return a projection of that aspect onto a categorical name
   that represents a physicalization of the aspect. This composes one of the
   inverse of one of the functions that is used to define a unit of the aspect
   with project-unit->categorical-name. If a scalar aspect is provided then the
   projection of any vector components of the aspect are left as an exercise for
   the executor."
  ; this version of this function does not take the enclosing black box as an
  ; argument, but probably has to
  (project-unit->categorical-name (aspect->unit aspect) proper-name))

;; prims
;; TODO figure out how to get these to show up correctly in protc
;; since they are priors in the language need a representation for them
;; in the language (should be possible to do this anyway ...)

;; quantified

(define count-listener (make-parameter "TODO"))
(define current-context (make-parameter "TODO 2"))
(define data-backend (make-parameter (λ (arg) (λ (aarg) (format "~a not implemented" arg)))))

(define (simple-data-store arg)
  (define store '((empty record)))
  (define (add record) (set! store (cons record store)))
  (cond [(eq? arg 'add) add]
        [else (λ (x) "WHAT HAVE YOU DONE!?")]))

(define (get-data-backend)
  ; TODO
  simple-data-store)


(define (get-current-context)
  ; TODO really we just want this to be the namespace of the current module
  universal-context)

(define (get-count-listener)
  ; TODO what to listen on for input
  ; be it a form or an input line etc.
  read-line)

(define (*count categorical-name enclosing-black-box #:implicit [implicit #f])
  "this is one of three fundamental functions mapping from being to symbol"
  ; if the count is defined as an implicit count then it means that this
  ; is an implementation detail that is only being modelled but that cannot
  ; be accessed directly and that the readout will not be computed directly
  ; from these count definitions, but instead be fed in as some kind of number
  (define (runtime-*count) 
    ; runtime will depend on the implementation
    ; there may be a way to require the spec at compile time
    ; so that the data dep is already known and can't be
    ; changed on the fly during execution, however being
    ; able to fail over might make sense, but I'm not sure
    ; that this is the correct interface to do that at ...
    (parameterize ([count-listener (get-count-listener)]
                   [data-backend (get-data-backend)]
                   [current-context (get-current-context)])
      (let ([value (count-listener)])
        ; this almost certainly needs to be done with call/cc
        ; or implemented using the actors that Mathias suggested
        (define proper-name (categorical->proper enclosing-black-box))
        (define t (current-milliseconds))
        (define protocol-execution-proper-name
          (execution-proper-name (context-execution current-context)))
        (define record (list protocol-execution-proper-name proper-name categorical-name value))
        ; we can then run (categorical-name->unit categorical-name proper-name) after the fact
        ((data-backend 'add) record))))
  runtime-*count)

;; unquantified
;; note that these aren't really unquantified, it is just that
;; we don't have access to the biological implementation of it
;; the old hot/cold big/small have dimensions but no units
;; the biology has to count _something_ to make the determiniation
;; but all that a human agent can report is the comparison

(define (make-substrate-category thing)
  ; there are a number (probably a very large number) of physical phenomena that 
  ; can form the basis for the creation of categorical names even before the
  ; you have to invoke the complexity of counting, or even of shorter/longer/etc
  ; something like relative motion is all that you need
  ; e.g. board a is less than board b if for all orientations of a and b when
  ; they are held lightly together, that b will move if both are turned upside down

  ; that is a very complex category definition, a simpler case would be
  ; "things that I can fit in my mouth" and "things that I cannot fit in my mouth"
  ; human babies go to great lengths to develop a procedural understanding of this category
  ; correlation of that category with something more abstract, such as the size aspect
  ; requires only that the definition of the function size allows the following to always be true
  ; (< (size can-fit) (size cant-fit)) which more correctly is
  ; (iff (and (can-fit? thing-1) (cant-fit thing-2)) (< (size thing-1) (size thing-2)))

  ; a different aspect would have to used for "moleculse I can hydrolize" and "everything else"
  ; for an enzyme, in fact, it might be only the aspect that they can be hydrolzied by
  ; that enzyme, but at some point some human might discover that that is because
  ; those molecules have made accessible a certain molecular boding structure etc.

  ; the reason that I bring these up here is because in some sense substrate
  ; categories are already implemented, they don't even need a symbolic representation
  ; in order to function, an enzyme is effectively an already implemented naming function
  ; for its substrates (clearly a confusing word choice here)
  (void))

(define (bbcons . black-boxes)
  "black box constructor can be treated as a spatiotemporal union sort of"
  void)

(define (substrate-count aspect proper-name)
  "the substrate is quantifying something but we don't have direct access to the units"
  ; even if we can't directly symbolize the scalar unit
  ; it is important to know its approximate resolution
  ; for determining the error bounds for equality i.e.
  ; the minimal distinguishable unit
  ; a concrete example of this would be that when we
  ; compare two length's visually we are counting some neural phenomena
  ; that is a function of the number of cones that are being activated and
  ; associated with each of the objects

  ; interestingly the idea of projecting a unit onto an undifferentiated being
  ; is interesting, because when a substrate unit is undefined it measn that
  ; we are trying to create an indefinite categorical name that we are then
  ; going to count, but the thing that is actually being counted is a neural
  ; phenomena, not the projected thing itself, so the question is whether there
  ; is a simple way to indicate that we really aren't counting anything in the world
  ; directly, and that the projection actually opperates in the opposite direction

  (define executor (get-current-executor))  ; TODO FIXME
  (define sensory-modality
    ; from the implementation of the protocol, or the prov
    ; FIXME TODO
    (hash-ref (context-category-alist current-context) 'sensory-modality))

  (define unknown-that-executor-is-counting
    (categorical-name (thing)  ; really categorical name is just lambda here
                      ; linearly-correlated is a macro that runs both procedures
                      ; under sufficiently variable conditions that multiple different
                      ; values of count can be obtained from the same enclosing black box
                      ; as defined here it probably couldn't pass an IRB review ^_^
                      (linearly-correlated (*count thing executor)
                                           (*count (project-aspect->categorical-name
                                                    aspect proper-name)
                                                   proper-name))))

  ; for completeness we use a black box constructor to explicitly state
  ; that this counting only makes sense when the proper-name and the executor
  ; are considered together, though the definition of the unknown might
  ; be sufficient to make that clear? there are a couple of ways we could do this

  ; NOTE that the #:implicit here prevents us from actually trying to execute
  ; unknown-that-executor-is-counting, but we do need a way to check and display the definition
  (*count unknown-that-executor-is-counting (bbcons proper-name executor) #:implicit #t)

  #; ; a useful concept, but we don't actually need it here since we have the unknown already
  (define-unit  ; temporary
    sensory-unit
    aspect
    (format "unit of ~a without a known symbolization where 1 sensory-unit
             corresponds to exactly 1 ~a" aspect unknown-that-executor-is-counting))

  #; ; old
  ((project-unit-onto-being-subset substrate-unit)
   (substrate-unit-minimal-distinguishable-when-quantified)

   (aspect+unit+black-box->being )
   (categorical-name projected-substrate-unit)

   phenomena-correlated-with-scalar-aspect-of-proper-name
   (*count projected-substrate-unit executor #:implicit #t)))

;; comparators
(define equality-listener (make-parameter (λ () null)))
(define greater-than-listener (make-parameter (λ () null)))
(define less-than-listener (make-parameter (λ () null)))
(define (get-equality-listener)
  ; TODO what to listen on for input
  ; be it a form or an input line etc.
  read-line)
(define (get-less-than-listener)
  ; TODO what to listen on for input
  ; be it a form or an input line etc.
  read-line)
(define (get-greater-than-listener)
  ; TODO what to listen on for input
  ; be it a form or an input line etc.
  read-line)

(define (*< aspect pn-1 pn-2)
  "unquantified less-than"
  ; any aspect here needs to be scalarizable for both proper names
  (define runtime-*= (*= aspect pn-1 pn-2))
  (define (runtime-*<)
    (parameterize ([less-than-listener (get-less-than-listener)])
      (and (not (runtime-*=)) (less-than-listener))))
  runtime-*<)
(define (*> aspect pn-1 pn-2)
  "unquantified greater-than"
  (define runtime-*= (*= aspect pn-1 pn-2))
  (define (runtime-*>)
    (parameterize ([greater-than-listener (get-less-than-listener)])
      (and (not (runtime-*=)) (greater-than-listener))))
  runtime-*>)

(define (*= aspect pn-1 pn-2)
  "unquantified equality"
  (define (runtime-*=)
    (parameterize ([equality-listener (get-equality-listener)])
      (equality-listener)))
  runtime-*=)

(define (*rank scalar-aspect . proper-names)
  ; At compile time this function should be able to read the specs for the proper names
  ; that it is being given and determine whether the scalar aspect can be obtained for
  ; each of them. This is essentially a type check. This needs to be implemented as syntax
  ; and it needs to be able to obtain the categorical name for each proper name. Probably.

  (define (runtime-*rank)
    void
    )
  runtime-*rank
  )
(define (*rank-vector scalar-aspect projection-rule . proper-names)
  void
  )

