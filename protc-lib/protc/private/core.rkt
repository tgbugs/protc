;;; core evaluation model
#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/pretty
                     racket/format
                     racket/syntax)
         racket/contract
         racket/generic
         racket/bytes
         syntax/srcloc
         "utils.rkt"
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
  ;(define-black-box )
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
#;
(
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
traditionally) on their location in the sky, since that depends on
earth's location which is the aspect of a proper name and thus

--rambling--
)
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

(define-for-syntax (make-value name)
  ;(println name)
  (format-id name
             #:source name
             "~a-value"
             (syntax-e name)))
(define-syntax (test-help stx)
  (syntax-parse stx
    #:literals (define)
    [(_ name:id (define (naming-function:id aspect:id ...) body:expr ...) ...)
     ;#:with ((aspect-value ...) ...) #'((aspect ...) ...)
     #:with ((aspect-value ...) ...) (map (λ (sl) (map make-value sl))
                                          (map syntax->list
                                               (syntax-e #'((aspect ...) ...))))
     #;
     (map (λ (a) (format-id a
                                                      #:source a
                                                      "~a?"
                                                      (syntax-e a)))
                                          (syntax->list #'(aspect ... ...)))
     (let ([out #'(list aspect-value ... ...)])
       ;(pretty-print (syntax->datum out))
       out
       )
     ]))
#;
(module+ test
  (test-help name-1)
  ;(test-help name-2 (define))  should fail
  (test-help name-3
             (define (nf-1 a-1 a-2))
             (define (nf-2 a-3 a-4)))
  (test-help name-2 (define (nf a-1 a-2)))
  (test-help name-2 (define (nf a-1)))
  (test-help hrm (define (nn a))))

(define-syntax (measure stx)
  "atomic measure"
  (syntax-parse stx
    [(_ being:id aspect:id)
     #:with aspect-name (datum->syntax #'aspect (symbol->string (syntax-e #'aspect)))
     #'(list (if-defined aspect
                         aspect
                         (begin (println (format "WARNING: aspect ~a is not defined" aspect-name)) null)
                         ) "lookup the spec for measuring that aspect for that thing if there is one")
     ]))

(struct name-data (name-string) #:transparent)

(define-syntax (define-name stx)
  (syntax-parse stx
    #:datum-literals (on-aspect)
    #:literals (any/c name-data)
    ; FIXME aspect isn't just an id, it is id for scalar, but (:: a-1 a-2) for vector
    [(_ name:id (~optional doc:string) (on-aspect (aspect:id ...) body:expr ...) ...)
     #:do ((add-being #'name) #;(displayln (~a "type-being-list:" type-being-list)))
     #:with name-string (datum->syntax #'name (symbol->string (syntax-e #'name)))
     #:with predicate? (format-id #'name
                                  #:source #'name
                                  "~a?"
                                  (syntax-e #'name))
     #:with ((aspect-value ...) ...) (map (λ (sl) (map make-value sl))
                                          (map syntax->list
                                               (syntax-e #'((aspect ...) ...))))
     #:with ((args-contract ...) ...) (map (λ (sl) (map (λ (s) #'any/c) sl))
                                          (map syntax->list
                                               (syntax-e #'((aspect ...) ...))))
     #:with (naming-function-temp ...) (generate-temporaries #'((aspect ...) ...))
     ;#:do ((displayln (~a "aspect-value:" (syntax-e #'(aspect-value ... ...)))))
     (let ([out #'(begin
                    (define name (name-data name-string))
                    (~@
                     ; TODO any/c should be name?
                     (define/contract (naming-function-temp aspect ...)
                       (args-contract ... . -> . boolean?)
                       ; FIXME body not sufficiently expressive?
                       ; or do we just use other predicates as free variables?
                       (let ([thing name])
                         ; allows body to pass name
                         ; have to use let due to (begin blah) issue
                         "hello!"
                         body ...))
                     ) ...
                    (define (predicate? thing)
                      ; this way of defining a predicate is NOT indended for
                      ; rendering as a protocol, because there is no ordering
                      ; nor anything else, need to deal with that elsewhere
                      ; when I ask predicate? during definition measure needs
                      ; to return a representation for export and runtime
                      (define aspect-value (measure thing aspect)) ... ...
                      ; TODO using and here is incorrect
                      ; have to introspect the body of the naming function
                      ; to determine if it is applicable in the current context
                      ; warn on missing aspects, etc
                      (and (naming-function-temp aspect-value ...) ...)))])
       #;
       (pretty-print (syntax->datum out))
       out)]))

(module+ test
  (require "aspects.rkt")
  (define (contains? thing value) #f)
  (define-name thing)
  ;(thing?)  ; fails, but do we want it to? yes probably
  (thing? "yes")
  (define-name hrm (on-aspect (a) "a body"))
  (define-name thing2
    (on-aspect (a b) #f)
    (on-aspect (nf2 c d) #f))  ; wtf
  ;(define-name thing3 (on-aspect (asp) 'very-good))  ; fails as expected
  (define-name thing3 (on-aspect (asp) #f))
  (thing3? "test")
  (define-aspect mam16se rna-seq-mammal-16s-equivalent
    "a genomic sequence that exists in all mammals that can be used to anchor primers whose
     contents can be used to determine which species the dna came from e.g. something like
     https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3865308/")
  ;(rna-seq-mammal-16s-equivalent)
  ; at this point we only need to know that the aspect is known
  ; and when this stuff is imported it should be looking up aspects
  ; in the top level environment, even when it does whether there is
  ; actual documentation for what the aspect means in that context
  ; is checked by this part of the code (which is in the overseer/orchstrator/conductor?)
  ; that is checked beforehand at the time the protocol is written
  ; and that will compile down to this even deeper representation
  ; ALTERNATELY we can do all the checking down here
  ; the values returned by (measure thing aspect) probably need restrictions??
  (define-name mouse
    (on-aspect (rna-seq-mammal-16s-equivalent)
      (contains? rna-seq-mammal-16s-equivalent "i am a mouse")))
  (mouse? "hohoh")
  (define-name ruler
    ; this doesn't work as well down at this level because the executor
    ; becomes the black box that evaluates a predicate
    ; augmenting with prov, such as "take a picture" could help, but
    ; that is another feature
    "the ruler should look like this"
    (on-aspect (similar-to-picture?)
               similar-to-picture?))
  
  (define-name bad-glass
    "a bad definition for glass"
    (on-aspect (transparency solidity)
               ; FIXME should this actually have to be defined on units?
               ; or if it is not then an implicit threhold function is implied?
               ; the problem here is that you want to know in context what
               ; type of value is expected for each aspect, which is why
               ; requiring the aspect to be defined is important, since
               ; measure aspect's results will depend on that :/
               ; the actual logic here cannot operate on dimensional aspects
               ; since a change in default units can completely change the acceptance criteria
               (and transparency solidity))
    #; ; predicate aspects vs something else
    (on-aspect (transparent solid)
               (and transparent solid))))

(module+ agent-oof
  ; oof what a mess, reveals pretty conclusively that
  ; define-name is not at all working the way we want it to

  ; placeholders to keep the compiler happy
  (define agent 'agent)
  (define public-key 'public-key)
  (define (lookup agent public-key) #f)
  (define (decrypt key sig) #f)
  (define signature 'signature)
  (define cypher 'cypher)
  (define (message-from-agent message) message)
  (define (encrypt key value #:scheme [scheme 'if-only-it-were-so-simple]) #f)
  (define overseer-public-key 'some-public-key)
  (define-syntax (loop-until-done stx)
    "just keep going"
    (syntax-parse stx
      [(_ body ...)
       #'(λ () body ...)
       ]))
  (define (send-to thing message return-address) #f)

  (define-name agent-bad
    (on-aspect (public-key random-value message-received)
               ; FIXME random value is not an aspect of the agent
               ; but of the whole situation
               ; furthermore the problem with this approach
               ; is that it is hard to define processive invariants
               ; as part of a name, there are other ways this could be encoded
               ; such as using agent-sent-random-value-encrypted-with-public-key
               ; but the higher level agent definition would be anything that
               ; when sent a random (putatively secret) value encrypted with
               ; the the public-key returned a message encrypting the random value
               ; with the private-key such that when decrypted with the agent-public-key
               ; and overseer-private-key the value matched the original random value
               ; the problem is that these definitions are purely functional predicates
               ; they can't be transactional UNLESS they make use of other naming functions
               ))
  (define-name agent-verification-black-box
    (on-aspect (public-key message-received random-value)
               ))
  (define-name agent-verification-black-box-that-wont-leak-random-value
    ; if these are symbolic aspects then this works just fine ... and we should switch back to define >_<
    ; except for the issue with types :/
    (on-aspect (agent-public-key overseer-private-key message-received random-value)
               )
    )
  (define-name message-received
    (on-aspect (address bytes-in-register))
    )
  (define-name message-from-agent-urg
    "Is the black box im communicating with the one that I think I am?"
    (on-aspect (signature content)  ; FIXME no way to differentiate the phase at which each of these is needed
               (define public-key (lookup agent public-key))  ; FIXME nasty free variables
               ;(knows-private-key-for-public-key)
               ;(has-ability-to-reproduce-a-value-i-thought-was-secret-that-i-made-public-after-encrypting-with-a-public-key)
               ; amusingly the difficulty I have encountered in expressing this is precisely because I'm actually asking the wrong question
               ;(define public-key "the public key that defines this agent")  ; lifted for category level
               (= (decrypt public-key signature) (cypher content))
               ; maintaining the secrecy of the message is now orthogonal
               ; since the message can be anything
               ))
  (define-name message-from-proper-agent-1
    (on-aspect (signature message)
               (parameterize ([public-key "proper-agent-1 public key"])  ; FIXME this is bad an annoying :(
                 (message-from-agent message-from-proper-agent-1)
                 )
               ))
  ; predicates don't quite work when you need an alist :/
  ; basically if I define a _local_ name by some value
  ; and I want to be able to reuse categorical or generic predicates the we need lookup
  ; if you define things every time then they work, but the information is trapped and you
  ; have to apply the predicate as a black box to everything
  ; more importantly define-name does not compose at all in the way desired
  (define (how-i-would-rather-do-it?)
    (define (symbolic-constraints agent-public-key overseer-private-key random-value return-address)
      ; return address is often left out of the equation >_<
      ; overseer-public-key could also be a one-time-pad or temp key etc.
      ; this is clearly wrong ... or I have no memory of what I was trying to do here
      ; ask them to decrypt with their private key and send me back the random value
      ; encrypted with the overseer public key ?? clearly vulnerable to the agent
      ; sharing the random value once they decrypt it, so would have to match
      ; something else to make this work ...
      (define message-sent (encrypt agent-public-key (cons random-value overseer-public-key)))
      "send message-sent to everyone and everything asking for them to return it!"
      "can't assume we have the agents address, they may actually come to us"
      "how to deal with the return address is another issue entirely"
      "some forms of communication (email) will always leak the return"
      (loop-until-done
       (define message-received (measure return-address next-message))
       ; remember kids, always encrypt with the other person's public key first!
       (= (decrypt agent-public-key (decrypt overseer-private-key message-received)) random-value)
       ))
    (void))

  ; all the free variables argh, I guess it is ok??
  (define (message-from-agent? signature message agent-proper-name)
    (define public-key (lookup agent-proper-name public-key))
    (= (decrypt public-key signature) (cypher message)))

  (define (message-listen address)
    "looking for some bytes here ..."
    (let ([signature 'lol]
          [message-received 'hahahaha])
      (values signature message-received)))

  (define (proper-agent-1? thing)
    ;(define random-value (random))
    ;(define message (encrypt public-key random-value))
    (define agent-proper-name 'magical-monadic-proper-name-lookup-device)
    (define return-address 'magical-monadic-return-addres-for-proper-agent-1-lookup-device)
    (define public-key (lookup agent-proper-name public-key))
    (send-to
     thing
     "hey, can you send me a message and signature encrypted with the private key for proper-agent-1?"
     return-address)
    (define-values (signature message-received) (message-listen return-address))
    (message-from-agent? signature message-received 'proper-agent-1)
    ; challenge response here ... if they have a private key and send you any message
    ; back and you have their public key then why do you need the random value at all!?
    )
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

#;
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

  (define (existence? [black-box null] [black-box-complement null])
    "accepts on anything including the null set but it cannot distinguish between
     arguments that cannot be expressed symbolically and reduces all of them to the
     empty set"
    #t)
  (check-true (existence?))
  (check-true (existence? null null))
  (check-true (existence? "thing" "context"))

  #;
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

;; data store records etc.
;; see https://docs.racket-lang.org/more/#(part._.Continuations)
;; for how to implement these using continuations
;; the dispatch table can allow us to transparently switch
;; between listeners if we construct the dispatch tag in a sane way

; these aren't sufficient to map back to the full execution context I think
; even if the execution id holds a pointer to the protocol, or maybe
; if we can use the proper name to look up other data associated with it it may be possible
;(struct record (execution-id timestamp proper-name categorical-name value) #:transparent)
;(struct record (execution-id timestamp proper-name aspect value) #:transparent)
(struct denormalized (protocol-version-id  ; aka hash + line number or deterministic hash on the source
                      execution-id  ; probably need execution sub ids if executor or other changes?
                      executor  ; for signed work need public key
                      categorical-name
                      proper-name  ; in a sense this could be a local proper name
                      aspect  ; TODO ah the joys of how to deal with multiple aspects and multiple values
                      value   ; also known as, what to do about opaque data file formats
                      timestamp  ; from agent? that would be the agent timestamp if it has one ... debug only?
                      aspect-agent  ; for signed work need public key
                      aspect-impl-used
                      inferred-prov ; this should all be coming from the protocol?
                      measured-prov ; if you have a smart measuring device 
                      aspect-agent-checksum
                      aspect-agent-signed checksum) #:transparent)

(struct pn-record (execution-proper-name ; only issue with leaving out protocol-proper-name is if a change was made on the fly ...
                   agent-proper-name  ; superclass of executor which includes machines, overseer etc
                   proper-name  ; we can factor categorical name into the proper name creation record
                   aspect
                   value))
(struct prov-record (agent-proper-name agent-signed-checksum)) ; probably in pn-record
(pn-record "example-1" "overseer" "example-1" "protocol" "the-protocol-proper-name")
(pn-record "example-1" "overseer" "(categories-for example-1)" "some-cat-name" "new-proper-name")
; this isn't quite right since it should be logging the actual functions, but could probably get that from
; the first record up there

; an alternative would be to actually store the individual measurement ids
; while this might seem like overkill, I think may map better onto the actual execution
; there is some weirdness in whether the agent needs to be able to verify or even know about
; the proper name IF that agent is not also the executor, which in many cases they will be
; all proper names and local proper names that are required by the protocol should
; probably be gathered at the begining of the protocol execution and mapped to the
; categorical and/or proper names in the spec and impl
(struct internal-record (measurement-id proper-name aspect execution-proper-name))
(struct payload (measurement-id value signature))
; encrypted version
(struct internal-record-e (random-measurement-id proper-name aspect execution-proper-name))
; approved agent for this measurement is sent random-measurement-id encrypted with public key
; only an agent holding the coresponding private key can send back the random measurement id
;(struct payload-e (agent-private-key-encrypted (overseer-public-key-encrypted (random-measurement-id value))))
; yes this conflates identity with encryption and is pgp thinking and has no forward secrecy
; the exact handling here should be tailored to the danger of what you are trying to measure
; most of the time the threat model is that sensors might get swapped due to line noise or something
; using agent-public-key here is tacitly the proper naming function


; file-path, file-format, and file-checksum are legitimate aspects
; the category of proper name for that would be the whole experimental
; setup including the actual subject of interest, but the transform below will be required
; allowing for non black boxed aspects that depend on those things
; is probably a good idea so that purely computational bits can be
; treated as if they were simply any other measurement ...
; the alternative is forcing a transformation to be defined on
; the data file which maps its values to the 'documentation only'
; aspects of the black box in question, so for example the average
; membrane potential from 1 second to 1.001 seconds would be
; lifted to the aspect representation from the data file
; this seems inefficient in practice, but would make the documentation
; clearer, rather than having to map the arcana that is a python script
; that analyzes ephys files, of course sometimes we want to pretend
; that our data doesn't correspond to names just quite yet so we use
; the sneaky "Region Of Interest" as a proxy (huh, that's an insight ...)
; the alternative of the transform would be a function that could translate
; patterns of access to the data file into their fundamental "what was counted"
; representation, allowing for the mathematical model between what was counted
; and the actual aspect you were trying to measure to be created explicitly
; a good example of this is be being able to account for junction potential
; simply by having the abstract model of the circuit and observing that
; that value is not accounted for explicitly in the protocol
; points to the need for "filled in from future result"
; or "calculated from first principles" the second being much easier ...

(define data-backend (make-parameter (λ (arg) (λ (aarg) (format "~a not implemented" arg)))))

(define (simple-data-store arg)
  (define store '((empty record)))
  (define (add record) (set! store (cons record store)))
  (cond [(eq? arg 'add) add]
        [else (λ (x) "WHAT HAVE YOU DONE!?")]))

(define (get-data-backend)
  ; TODO
  simple-data-store)



;; listeners
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


(define count-listener (make-parameter "TODO"))
(define current-context (make-parameter "TODO 2"))

(define (get-current-context)
  ; TODO really we just want this to be the namespace of the current module
  universal-context)

(define (get-count-listener)
  ; TODO what to listen on for input
  ; be it a form or an input line etc.
  read-line)


;; quantified

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

