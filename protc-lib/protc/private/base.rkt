#lang racket/base
(require debug/repl (for-syntax debug/repl
                     racket/pretty
                                ))


(require (for-syntax racket/base
                     ;racket/class
                     syntax/parse)
         (except-in racket/base time)  ; FIXME how to address this?
         (prefix-in racket: (only-in racket/base time))
         racket/class
         racket/syntax
         racket/string
         syntax/parse
         racket/dict)

(require (for-meta 0 racket/base)
         (for-meta 1 racket/base)
         (for-meta 2 racket/base)  ; needed for if-defined to work...
         ;(for-meta 3 racket/base)
         ;(for-meta 4 racket/base)
         )

(require "syntax-classes.rkt"  ; vs protc/private/syntax-classes
         "utils.rkt"
         (for-meta -1 protc/private/syntax-classes)
         (for-syntax protc/private/syntax-classes
                     (only-in "utils.rkt" name-join))
         (for-template protc/private/syntax-classes)
         )

(provide spec impl :
         ;(rename-out [#%make make] [#%measure measure] [#%actualize actualize])
         ;#%make
         ;#%measure
         ;#%actualize
         ; for utility, should not be required by base... probably need to reorganize
         :* *: **
         actualize measure make
         define-message
         ;(for-syntax name-join)
         (for-syntax (all-from-out racket/base))
         )

(module+ other

  (define (specf #:type type
                 #:name name
                 #:aspect [aspect null]  ; again a reason to want a language...
                 #:uses [uses '()]
                 #:vars [vars '()]
                 #:inputs [inputs '()] 
                 #:invariants [invariants '()]  ; this is where this approach is annoying, we want to be able to have .invariant .invariant
                 #:prov [prov '()])
    (filter (λ (v) (not (null? v)))
            (list name type aspect
                  vars
                  inputs  ; here we have no fancy .inputs name :kg/m 'value
                  invariants  ; TODO when not using syntax we have to resolve inputs and vars manually :/
                  prov
                  ))
    )

  ; observe the drawbacks :/
  (specf #:type 'actualize  ; this should be bound
         #:name 'cut-down-to-size  ; this is a name we are doefining
         #:aspect mass
         #:uses 'weigh
         #:vars '(initial-weight final-weight)  ; and here we see another problem these need to be variables
         #:inputs '(knife)
         #:invariants '((< final-weight initial-weight))
         )

  ;; database setup
  (define (setup-counter)
    (define ic 0)
    (λ () (let ([current-value ic])
            (set! ic (add1 ic)) current-value)))

  (define (setup-db)

    ; tables, indexes, and sequences
    (define being-index '((being . data)))
    (define aspect-index '((aspect . data)))
    ;(define messages '((aspect . data)))

    (define being-counter (setup-counter))
    (define aspect-counter (setup-counter))

    (define being-records '((-1 . ())))
    (define aspect-records '((-1 . ())))

    (define (add-being name record)
      (add-rec name record #:type 'being))

    (define (add-aspect name record)
      (add-rec name record #:type 'aspect))

    (define (add-rec name record #:type type)
      (let-values ([(index counter records)
                    (cond [(eq? type 'being) (values being-index being-counter being-records)]
                          [(eq? type 'aspect) (values aspect-index aspect-counter aspect-records)]
                          [#t (error "type unkown")])])
        (if (dict-ref name index)
            (error "that being is already defined...")
            (let ([current-index (counter)])
              (set! index (cons (cons name current-index) index))
              (set! records (cons `(,current-index . ,record) records))
              (void)))))

    (define (dump-recs) (list being-index aspect-index being-records aspect-records))

    (values add-being add-aspect dump-recs))

  (define-values (add-being add-aspect dump-recs) (setup-db))
  )

;;; implementation of protc forms using only racket syntax tools

;; new syntax classes that we are defining for protc
; used to enforce section ordering and structure during pattern matching
; see https://docs.racket-lang.org/syntax/stxparse-specifying.html#(part._stxparse-attrs)

(module+ thoughts
;; section
#'(name:section-name header:section-header body:messages)

;; section-header probably want to break beings into their own thing...
; as it exists now
#'((or *being* *aspect aspect* aspect) variable-name:id granular-imports)
;generic
'(spec (_ *:aspect)
        ; 99% of these should be specified generically since they are specifications
        ; the other 1% it would be 'suggested domain of restriction' eg (mouse *:mass) but that is really an implementation
        ; anything that then .uses aspect should be able to fill in automatically
        ; as should any aspects defined to inherit from it
        (.input _ ...)
        (.symret _ :aspect))
; the other option is to drop the (_ because .input is much clearer for binding names, though (_ is terser when
;   dealing with a single black box... maybe we can support both? in thise case (_ is really the output... if it were a aspect* step...
'(spec (_ *:g (.uses *:mass))
        ; should be able to be applied to any derived aspects like :kg transparently and automatically
        (.input _ ...)
        (.symret _ :g)  ; symbolic return should automatically be lifted to 'collect' or 'measure'
        )

#'(spec (_ :aspect*)  ; remember in our evaluation model we have to wait until we bind the world... so (_ may not be relevant?
        ; sections of this form actually seem kind of useless
        ; whey they don't have real values...
        (.input _ :aspect -_-))
#'(spec :aspact*
        (.input _ :aspect value))
; and once you figure it out, the symmetry is perfect
#'(spec (:aspact* value)
        (.input _ :aspect value))
; OR we use :aspect* to specify how .inputs name :aspect1 value :aspect2 value2 are individual to be actualized...
; because that is nice shorthand for (.parameter* name :aspect value)
; the real meat is in impl for :aspect*
; generic spec :aspect* sections do not exist...
; specific spec :aspect* definitely do for precision of the final result etc...
; actually wait... NO still not the case, because we have syntax
; for that in the (spec *being*) form... except in the generic case...
#'(spec ((actualize :aspect) spec-name)
        (.parameter :aspect (range 0 1))  ; functional parameter on the returned measurements
        )
#'(spec :ug* NIST-accurate-ug
        (.invariant (<= (error *:ug) 0.001)))  ; when an actual value is present it needs to be scaled if the prefix changes
#'(spec drug-micro-dose
        (.inputs drug-powder :NIST-accurate-ug 10
                 a-tiny-robot
                 ))
'(impl (_ *:g weigh-with-scale)  ; vs (impl *:g weigh-with-scale because the primary input is the same as the output
                                  ; *-:g  destructive measure  *':g modifying measure
        (.inputs scale ...)
        (.outputs scale ...) ; conserved inputs
        (.symret _ :g))
'(impl _ generic-weigh (.uses weigh-with-scale)  ; this is a generic implementation, the output name is _ so the impl can be applied
        (.inputs thing-input weigh-boat)
        (a-on-b weigh-boat scale)  ; we can skip put or can translate directly
        (part-of zero-button scale)  ; executors need to know what push means or we just button as a verb directly or just 'use'
        (use zero-button)  ; this should raise a "no how" error which a (spec (use *-button)) should be able to fix...

        (subset/made-up-of ([t thing-input] #:when (a-on-b? t weigh-boat)) t)
        (.outputs thing-input' ...))

#'(impl drug-micro-dose the-dumb-way (.uses weight-with-scale)
        ; black box name constructor functions are the only way to get local names without defining them
        ; as inputs because they may be implementation dependent and they will be lifted to inputs
        (.inputs weight-boat)
        (part-of powder-grain drug-powder)  ; must have at least one bound symbol when calling these *a<b* ?? referential only...
        ; made-up-of-many-things-all-being-called-the-same-thing-but-not-always-actually-the-same
        (result powder-grain :diameter (range (qnt 0.2) (qnt 1 (unit 'meter 'milli))))
        (contained-in drug-powder container)  ; *a<-i-b*
        ;(subset #:condition? a-on-b drug-powder scale)  ; subset 
        ;(on (subset drub-powder) scale)  ; subset should auto track (subset (a-in-b drug-powder weigh-boat))
        (subset ([t thing]) )
        (steps ((use a-tiny-robot) ())
               (weigh-with-scale)
        ))

;; black box subsets by partitioning rule
 ; alternate is (subset ([t (rule thing)] ... 
 ; compare to for for/list
 ; subset/rule seems like it would compose with (for just fine
 ; could make a for-subset/rule or something...
 ; actually... because there are cases where someone might
 ; want to apply a procedure to _make_ the subset...
 ; or is this too much for this...
 ; for the time being this just for allow precise naming
 ; "for each slice of the carrot put a toothpick in it"
 ; could be expressed as (do ([s (slice carrot)] #:when (< s :mm-thick 7)) (a-insert-b toothpick s))
 ; this will produce some number of carrot-slice-toothpicks 
(define-syntax (subset stx)
  #'(subset/rule ([t thing] #:when (predicate? t other-thing)) ))

#'(impl (_ :aspect* value-spec))
#'(impl (:aspect* impl-name #:restrictions 'my-restrictions) messages)  ; arent restrictions the spec? also (.applies-when 'condition)
#'(impl ((actualize (aspect aspect-name)) name
                                          #:for-spec spec-name  ; #:implements spec-name
                                          #:restrictions 'my-restrictions) messages)

; this works for generics, but what about when we get to the point where we are trying to specify more complex things?

;(_ *:aspect) expansions... more like, we actually need a special header for each section or something...


'(
(_input *:aspect) -> (measure _input (aspect name))  ; this way to make it possible to call measure once when many *: are called
(_input *:aspect) -> (measure _input (aspect aspect))  ; we can work with this... can protc:aspect to keep names safe

(_input :aspect*) -> (actualize (with-parameter* ([aspect value-from-body]) _input))

)

;; body:messages
; do we need to be able to make defining the accepted value types for messages part of the language?
; does single-expression in the context evaluate if not quoted? need to test, also values
#'(message-name:message-type message-value:single-expression)
#'(invariant thing:defined-in-inputs-or-imports aspect-name:defined-aspect [message-name:message-type [message-value]+]+)


;; defining the symbolic components
; Translates nicely to using def for the purely symbolic parts
; eg
; def :aspect-name words if a given, otherwise operations on other aspects or a spec section combining...
; def .message-name
; def *being-name*
; def function  ; juse reuse define...

'(define-measure aspect-name spec-name-or-use-_-for-generic #:type 'non-destructive #:uses)
'(define-actualize aspect-name spec-name-or-use-_-for-generic #:type 'non-destructive #:uses)

;; define-measure
;spec *:aspect [_blank/generic]
;; define-actualize
;spec :aspect* [_blank/generic]
;spec _blank/generic [*:aspect :aspect*]

;; define-black-box
;spec output-name [(subClassOf thing)]

; implement
;impl _blank/generic

; the thing is that this doesn't work because we don't know ahead of time
; how many bound or unbound identifiers there are...
'(let-values ([() ()])
  (let ([spec-id-1]
        [spec-id-2])
    (let ([impl-id-1]
          [impl-id-2])
      (let))))
)
; MODULE END


(define-syntax (define-black-box stx)
  (syntax-parse stx
    [(define-black-box name:id body:expr)
     #'(define name (list body))  ; simple for now
     ]))

(define-syntax (define-aspect stx)
  (syntax-parse stx
    [(define-aspect name:id body:expr)
     #'(define name (list body))  ; simple for now
     ]))

(define-syntax (-define-message outer-stx) ; dead
  (syntax-parse outer-stx
    [(define-message (name:id spec) body:expr)  ; TODO spec:message-spec
     #'(define-syntax (.name stx)  ; may need to use syntax->datum to add the .
         (syntax-parse stx ;#:literals (spec)
           [(_ spec)
            #'()]))]))

(define-syntax (define-message stx)
  (syntax-parse stx
    #:literals (begin
                pattern
                define-syntax-class)
    [(_ mess:message-struct-sc (~optional doc) body-syntax-template)
     (println `(help!: ,(syntax->datum #'(mess.body-pattern ...))))
     #'(begin
         (define-syntax-class mess.sc-name
           ; TODO add the dot out front ala .message-name
           (pattern ((mess.name mess.body-pattern ...))))
         ;(impl-spec-add-message message-name #'local-sc-name)  ; TODO more args here
         (define-syntax (mess.name stx-i)
           (syntax-parse stx-i
             #:literals (....) ; FIXME
             [(_ mess.body-pattern ...)
              ; can't use the syntax class from above
              ; because then the references would be mymess.thing-defined-template
              ; and we don't want to have to write #'(mymess.thing) in the template
              body-syntax-template
              ]))
         (provide mess.name mess.sc-name)
         )]))

(define-message (vars free-variable:id ...)
  ; and then we can use write a macro that wraps parameterize
  #'(λ () (define free-variable (make-parameter null)) ... (void)))  ; FIXME nothunks?

(define-message (inputs local-name:id ... aspect-expr:asp ... (~optional ....))  ; FIXME asp-exp instead?
  #'(λ () (define local-name 'TODO-some-real-thing) ...  ; FIXME no thunks, need to figure out what causes begin to fail
       ; the alternative here is to use let instead.... (let ([local-name 'some-thing-TODO] ...) future-body)
           ; FIXME need a better way to check if the name exists
           aspect-expr.aspects ... ; asp has nested
           ;aspect-expr.aspect-value ... ...
           (define aspect-expr.thing 'TODO-some-real-thing) ...
           ; TODO inject the invairants...
           (println (list local-name ... aspect-expr.thing ...))
           (void)))

(define-message (outputs defined-name:id ... aspect-expr:asp ... (~optional ....))
  "This is not an actual output in the sense that it is not 'returned' by the function.
Rather, it is a way to say what _should_ be output, and to place constraints on those
outputs. Specifically when actualizing something like concentration we need to know
how what it is bound to because it could be bound as a constraint on an input.
"
  #'(λ () "TODO need to figure out how to push these constraints into rosette
and how to combine them in an impl section" (void))
  )

(define-message (invariant body:expr)
  #''(rosette-assert body))

(define-message (order step ...)  ; FIXME should be step:id
  #'(list step ...))

(define-message (order+ step ...)
  "Order steps and link the output of one step to the inputs of the next"
  #'(list 'compose step ...))  ; TODO

(define-message (identifier (~seq type ident) ...)
  ; FIXME figure out the best syntax and move on
  ; KISS for these things, no alternate forms
  #''((type . ident) ...))

(define dc-list '())

(define (delegated-concepts . rest)
  (if (null? rest)
      dc-list
      (set! dc-list (append rest dc-list))
      ))

(define (ont-term curie)
  ; TODO heh
  curie)

(provide delegated-concepts ont-term)

(define-message (delegate (~seq concept (~optional ident)) ...)
  ; FIXME executor
  #'(λ ()
      (define concept (if ident ident (symbol->string (syntax->datum concept)))) ...
      (delegated-concepts concept ...)
      (displayln delegated-concepts)
      ))

; *aspect says I'm going to measure this (measure aspect)
; aspect* says I'm going to actualize this (actualize aspect)
; aspect says I'm going to define this based on other aspects (define aspect)
;  or define it as a fundamental quantity aspect, id something like the kilogram
;  all other actual measurement devices used in *aspect sections






(struct step (name [spec #:mutable] [impl #:mutable]) #:inspector #f); #:mutable #t)

(define step%
  (class object%
    (inspect (make-inspector))  ; and this is how to let your objects be not opaque!
    (init name)
    (init [spec this])  ; FIXME this needs to be exracted to an impl class
    (define -name name)
    (define -spec spec)
    (super-new)
    "Step objects. Probably will be broken into impl and spec since there are slightly different needs."
    (define -impls '())
    (define -vars '())
    (define -inputs '())
    (define -invariants '())
    (define -parameters '())
    (define -outputs '())
    (define docstring "")
    (define/public (.name) -name)
    (define/public (.docstring [doc null])
      (if (null? doc)
          docstring
          (begin (set! docstring (string-append docstring doc)) docstring)))
    (define/public (add-impl impl) (set! -impls (cons impl -impls)))
    (define/public (def . things) 'pls-go)  ; FIXME this should not be here :/
    (define/public (.delegate . things) (void))  ; TODO! local to global lifting for this is probably appropriate
    (define/public (.identifier id) id)  ; TODO make it possible to add these dynamically, probably by spinning up a child class...
    ;(define/public (.input name [aspect null] [value null]) -name)  ; atomic structure of the message? ick
    (define/public (.vars . things) (void))  ; atomic structure of the message? ick
    (define/public (.inputs input-struct) (set! -inputs (cons input-struct -inputs)))  ; atomic structure of the message? ick
    (define/public (.outputs do-not-want) (void))  ; maybe we want this as a concise way to bind things
    (define/public (.invariant . things) (void))  ; copy me to make more! TODO this cannot be allowed in the impl section...
    (define/public (.parameter . things) (void))  ; not clear if we actually need this?
    (define/public (.order . things)  ; order on making inputs and/or on individual actions, practical deps should resolve automatically
      (void))
    (define/public (.order+ . things)  ; link input/output of generic procedures
      (void))
    (define/public (practical-order . things) (void))  ; how to actually achieve things not what is known to matter scientifically
    ; TODO how to do temporal restrictions on in place steps... are there in place steps??!
    (define/public (func) (void))  ; copy me to make more!
    (define/public (.echo message) `(echoing: ,message))
    (define/public (.ping) 'pong)

    ;(define/public (.add-spec spec) spec)
    ;(define/public (.add-impl impl)
      ; validate in the context of the existing spec ??
    ;impl)
  ))

(define new-step (new step% [name 'new-step]))

; placeholders to keep syntax-parse happy
(define-syntax (#%measure stx) #'(void))  ; aspect name

(define-syntax (#%actualize stx) #'(void))  ; aspect name 

(define-syntax (#%make stx) #'(void))  ; name

(define-for-syntax (pw stx)
  (when #f (display "syntax: ") (pretty-write (syntax->datum stx))))

; message preprocessing boom extensiblity
(module message-proc racket/base
  ; TODO redefine all of these as syntax classes
  ; using a define-message macro
  (require (for-syntax racket/pretty racket/base syntax/parse))
  (require "syntax-classes.rkt" (for-syntax "syntax-classes.rkt"))
  (provide (all-defined-out))
  (define-for-syntax (pw stx)
    (when #f (display "syntax: ") (pretty-write (syntax->datum stx))))

  ;; begin defining message syntaxes
  (define-syntax (#%.delegate stx) (pw stx)
    (syntax-parse stx
      [(#%.delegate v:expr ...)
       #'(quote (v ...))]))

  (define-syntax (#%.identifier stx) (pw stx)
    (syntax-parse stx
      [(#%.identifier v)  ; TODO #:type [type 'iri] etc..  ; ideally v:str but also str:literal for quotes...
       #'v]))

  (define-syntax (#%.vars stx) (pw stx) #''vars
    (syntax-parse stx
      ;#:literals (: aspect-as-id) 
      ;[(#%.vars v:aspect-expression)
       ;#'(aspect-as-id (: ))]
      [(#%.vars v:expr ...)  ; TODO v:id and aspect:expr :/
       #'(quote (v ...))]))

  (define .... 'more-to-come)
  (define-syntax (#%.inputs stx)
    ;(pw stx)
    (syntax-parse stx
      #:literals (....)  ; FIXME ... not ....
      [;(_ local-name:id ... [local-name-inv:id invariant:expr] ... .... )  ; FIXME this needs its own syntax class
       (_ local-name:id ... aspect-expression:asp ...)
       #'(λ ()  ; FIXME we need to be able to inject these names into a closure
           ; FIXME messages probably need to be syntax classes
           ; so that input.name ... can be accessed from outside
           (define local-name 'should-be-a-class?) ...
           (define aspect-expression.thing 'should-be-a-class?) ...
           (void))]))

  (define-syntax (#%.outputs stx) (pw stx) #''output)  ; FIXME probably should not be done the way that requires this... the name in the make should be the output...

  (define-syntax (#%.invariant stx) (pw stx) #''invariants)

  (define-syntax (#%.order stx) (pw stx) #''invariants)  ; should be used to specify the order in which to make named inputs

  (define-syntax (#%.order+ stx) (pw stx) #''invariants)

  (define-syntax (#%def stx) (pw stx) #''invariants)  ; FIXME DO NOT WANT
  )

(require 'message-proc)

(module+ test
  (define ms% (new step% [name 'ms]))
  (#%.vars a b c d e)
  (send ms% .echo (#%.vars hello))
)

(define-syntax (#%message stx)
  (pw stx)
  ;(displayln "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
  (let ([output
         (syntax-parse stx
           #:literals (quote
                       delegate)
           ; not actual messages TODO handled before we get here??
           ;[(_ target (delegate body:expr ...))  ; FIXME should catch earlier to lift terms?
            ;'#(delegate body ...)]

           ; TODO use target to make the name local? also, will local-def.body evaluate correctly here?!
           [(_ target local-def:def-sc) #'(define local-def.name local-def.body)]
           [(_ target doc:str) #'(send target .docstring doc)]
           [(_ target (quote thing)) #'(send target .echo (quote thing))]  ; send quoted stuff directly as a literal

           ; messages
           [(_ target (message-name:id message-body:expr ...))
            ; this will throw weird errors if a quoted expression is in the body... probably need to fix
            (with-syntax ([process-message (name-join "#%" #'message-name stx)])
              ; FIXME we need to be able to define here! .inputs specifically :/
              ;#'(send target message-name (process-message message-body ...)))])])
              #'(send target message-name (message-name message-body ...))  ; TODO error handling here so we can get line numbers
              )])])
        (pw output)
        output))

(define-syntax (--def stx)  ; dead
  "Define a local identifier using a single expression.
For example use @(def solution (a+b solute solvent))."
  (syntax-parse stx
    [(_ name:id body:expr)
     #'(define name (quote body))]))

(define name #''placeholder-for-class-syntax)
(define add-impl #''placeholder-for-class-syntax)

(define-syntax (spec stx)
    ; (spec (measure (aspect g) weigh) ...)
  (let ([output 
  (syntax-parse stx #:literals (spec  ; note that syntax-case doesn't work with syntax classes :( so body:expr fails :/
                                ;#%measure
                                ;#%actualize
                                ;#%make
                                name
                                ; delegate
                                def)


                [(_ (~or* section:actualize-sc section:measure-sc) message:impl-spec-body-sc ... )
                 #'(begin section.aspect
                          (if-defined section.name
                                      (void)
                                      (define section.name (new step% [name 'section.name])))
                          (#%message section.name message) ... )]

                [(_ section:make-sc message:impl-spec-body-sc ... )
                 #'(begin (if-defined section.name
                                      (void)
                                      (define section.name (new step% [name 'section.name])))
                          (#%message section.name message) ... )]

                #|
                [(_ ((~or* #%measure #%actualize) aspect:id -name:id) (~alt (def local-name:id body:expr) message:expr) ...)  ; TODO aspect-name:aspect
                 ; TODO check whether the aspect exists and file the resulting
                 ;  spec as part of it or similar
                 #'(if-defined -name
                               (begin aspect  ; check to make sure the aspect has been defined
                                      (def local-name body) ...
                                      (#%message -name message) ...)
                               (begin aspect
                                 (define -name (new step% [name '-name]))  ; TODO append % to name -> name% automatically...
                                 (def local-name body) ...
                                 (#%message -name message) ...))]
                ;[(_ (#%actualize aspect:id -name:id) message:expr ...)  ; TODO body:messages
                 ;#'(if-defined -name
                               ;(begin (#%message -name message) ...)
                               ;(begin
                                 ;(define -name (new step% [name '-name]))  ; TODO append % to name -> name% automatically...
                                 ;(#%message -name message) ...))]

                [(_ (#%make -name:id) (~alt (def local-name:id body:expr) message:expr) ...)
                 #'(if-defined -name
                               ;(set-step-spec! name% (list 'being (#%message name% message) ...))
                               (begin
                                 (def local-name body) ...
                                 (#%message -name message) ...)
                               ;(define name (step 'name (list 'being body ...) '())))]))
                               (begin
                                 (define -name (new step% [name '-name]))  ; TODO append % to name -> name% automatically...
                                 (def local-name body.) ...
                                 (#%message -name message) ...))]
                |#
                )])
    (pw output)
    output))

(define-syntax (impl stx)
  (let ([output
         (syntax-parse stx #:literals (impl
                                       ;#%measure
                                       ;#%actualize
                                       ;#%make
                                       add-impl
                                       send quote case
                                       )
                       [(_ section:impl-sc message:impl-spec-body-sc ...)
                        #'(if-defined
                           section.spec
                           (begin
                             (if-defined section.name
                                         (void)
                                         (begin
                                           (when (eq? (quote section.spec)
                                                      (quote section.name))
                                             (begin
                                               (printf
                                                "WARNING: line ~a: defining the default impl for ~a\n"
                                                (syntax-line section)
                                                section.spec)))
                                           (define section.name (new step%
                                                                     [name 'section.name]
                                                                     ; TODO need to decouple
                                                                     ; into impl% class
                                                                     ; going to take a bit of
                                                                     ; thinking for how to do it
                                                                     [spec section.spec]))
                                           (send section.spec add-impl section.name)
                                           ))
                             ; FIXME (def ... will not quite work as desired here becuase the defines
                             ; will go into a global namespace
                             (#%message section.name message) ...)
                           (raise-syntax-error 'impl-before-spec
                                               (format
                                                "No specifiction exists for ~s. Please create that first."
                                                'section.spec)))])])
    (pw output)
    output))

(define-syntax (delegate stx)
  ; TODO have defined the local .delegate version maybe use this for global?
  "delegate this particular term/block to the current executor"
  (syntax-parse stx
    [(_ name:id body:expr ...)
     #'(if-defined name
                   (set! name (append name (list body ...)))
                   (define name (list body ...)))]))

;#'(let ([rec (list 'being body ...)])
;(if-defined name
;(set-step-spec! rec)
;(step 'name rec '())))]))

;#'(define name (let ([rec (list 'being body ...)])
;(with-handlers ([exn? (λ (exn) (set-step-impl! rec))])
;(step 'name '() rec))))]))

;; inline results desired behavior... because one use case is definitly in a closed loop where prov is not rigorous
; (result 'self-evaluating-value 'self-evaluating-reference-to-protocol prov-identifier)
; (define (prov res:f9fe4133610611385d1aaab628a04aa122080c2c))  ; or however we are going to do that...
; pcb length (result prov) -> pcb :length (result prov 100 mm)
; (define (hprov "https://hyp.is/hy0bDnC0Eeer3jcZ6Qvypw"))
; images count (result hprov) -> images :count (result hprov 1850)
; cows-on-the-moon count (result 'bullshit 100000)
; pcb :thickness (result (measure-with :mm "30 cm ruler") 3)

;; searching for counterexamples where black-box :aspect would not be valid for our strict definition of black-box
; i think the answer is that there aren't any becuase the ones that might fail just end up being zero
; head-of-a-pin :number-of-butterflies-on-thing -> 0
; as a thought experiment, is there any real black-box where the expression black-box :volume would be invalid?
; what is (part-of intestines sperm-whale) :volume ? If you tried really hard you could probably come up with an answer
; how about "the average time a pod of orcas in the galapagose spends diving between surface stops?"
; have to refine to "what is the average time interaval between sighting orcas on the surface"
; (observe (a-near-b pod-of-orcas (bounds surface water-of-ocean)) time 
; (observe (not (empty? (subset/a-in-b (part-of any (member-of any pod-of-orcas)) air-above-the-water)))) time
; time-start time-stop etc, need a way to specify more about the context of the observations
; any and all could be special keywords...
; what is the average time interval from the first surfacing to the last surfacing
; what is the average time interval from the first surface to max on surface at same time
; what is the average time from max on surface to none on survace for > time on surface?

(module+ test
  (define volume 'volume)
  (define brain 'asdf)
  (define thing 'wat)
  (define my-thing 'other-wat)
  (define g 'grams)
  (define m 'meters)

  (syntax-parse #'(: g 10 m 12)
    [a:are-you-kidding-me
     #'([a.aspect a.value] ...)])

  ;(syntax-parse #'(: g) ; b [c 1])  ; unused
  ;[a:asp  ; note to self, syntax classes surrounded by () dont need an extra pair ala (a:asp)
  ;#'a.aspect])

  (syntax-parse #'(: my-thing g ([volume 10])) [a:asp
                                                ;#'(a.thing a.aspect ... [a.aspect-value a.value] ... )
                                                #'(a.thing a.aspects a.values)
                                                ])
  (syntax-parse #'(: my-thing g kg mM ([volume 10] [voltage 9999]))
    [a:asp
     ;#'(a.thing a.aspect ... a.aspect-value ... a.value ... )
     #'(a.thing a.aspects a.values)
     ])
  (syntax-parse #'(: my-thing g kg mM ([volume 10] [voltage 9999]))
    [a:asp
     ;#'(a.thing a.aspect ... (a.aspect-value a.value) ... )
     #'(a.thing a.aspects a.values)
     ])
  (syntax-parse #'(: my-thing g ([volume 10])) [a:asp #'a.aspects ])
  (syntax-parse #'(: my-thing g g g g g ([volume 10])) [a:asp #'a.aspect ])

  g
  (: my-thing g)

  (define-syntax (test-asp stx)
    (syntax-parse stx
      [(_ a:asp)
       ;#'(list a.aspect ... )]
       ;#'(list a.thing a.aspect ... (list a.aspect-value a.value) ...)
       #'(list a.aspects a.values)]
      ))

  ;(test-asp (: g))  ; no longer relevant
  ;(test-asp (: brain g))  ; FIXME
  (test-asp (: brain g ([volume 5])))
  (test-asp (: brain g ([volume 5] [volume 1000])))
  )

(module aspects racket/base
  ; It is important to distinguish between aspects as aspects and aspects as units
  ; this is not the ultimate representation that we want for aspects either...

  ; TODO I think that types for aspects and beings will help a whole lot and will be much faster to check
  ;  at compile time than at run time...
  ; TODO these need to be reworked to support si prefix notation etc...
  ; they also need to implemented in such a way that it is natural for define-aspect to add addiational aspects
  ; finally in the interim they probably need to support (: thing aspect ([aspect value])) syntax...
  ; it also seems like there aren't that many cases where having programatic access the actual unit names is going to be needed?
  ; so inside of (: could default to not needing to quote, and have another special form (-: (lol) or something that
  ; allowed programic access
  ; (aspect vs (aspect-variable or something
  ;(require (except-in racket/base time))  ; sigh, how to fix
  (provide (all-defined-out))
  (struct aspect (shortname name def parent)  ; note that #:auto is global...
    ; aka measurable
    #:inspector #f)

  ;(define unit (aspect 'unit 'unit "Units are not aspects but they can be used as aspects"))  ; units are not aspects their names can be...
  ; TODO define all these using (define-aspect)
  (define fq (aspect 'fq 'fundamental-quantity "The root for all fundamental quantities" 'root))

  ;(define :scalar () ()) ??
  (define count (aspect 'count 'count "How many?" fq))  ; discrete continuous
  (define % (aspect '% 'percent "portion out of 100" count))  ; FIXME ratio
  (define mass (aspect 'mass 'mass "The m in e = mc^2" fq))
  (define energy (aspect 'energy 'energy "hoh boy" fq))  ; TODO synonyms... distance...
  (define length-aspect (aspect 'length 'length "hoh boy" fq))
  (define time-aspect (aspect 'time 'time "tick tock" fq))
  (define temperature (aspect 'temp 'temperature "hot cold" fq))
  ; FIXME charge is actually NOT fundamental either... it is the count of the number of electrons
  ; times the elementary charge (heh)
  (define charge (aspect 'Q 'charge "hoh boy" fq))  ; why is it current??? http://amasci.com/miscon/fund.html

  (define dq (aspect 'dq 'derived-quantity "A quantity derived from some other quantity" 'root))
  (define current (aspect 'I 'current '(/ charge time-aspect) dq))  ; TODO expand quoted definitions
  (define weight (aspect 'weight 'weight "hrm..." mass))
  (define distance (aspect 'distance 'distance "hrm..." length-aspect))

  (define area (aspect 'area 'area '(expt length-aspect 2) dq))
  (define volume (aspect 'vol 'volume '(expt length-aspect 3) dq))

  (define duration (aspect 'dt 'duration '(- time-aspect time-aspect) dq))

  (define mol (aspect 'mol 'mole "HRM" count))

  (define _m 1e-3)

  (define l (aspect 'l 'liters "SI unit of volume" volume))
  (define L l)  ; TODO alternate forms that also have 'L as the short name (for example)
  (define ml (aspect 'ml 'milli-liters "very small liters" '(* _m l)))  ; FIXME all of this needs to be done in rosette
  (define mL ml)
  (define g (aspect 'g 'grams "SI unit of weight" mass))

  (define concentration (aspect 'concentration 'concentration "concentration" '(/ count volume)))
  (define M (aspect 'M 'molarity "SI unit of concentration" '(/ mol L)))  ; FIXME HRM... mol/volume vs mol/liter how to support...
  (define mM (aspect 'mM 'milli-molarity "SI unit of weight" '(* _m M)))  ; TODO auto prefix conversion because this is icky
  )
(require 'aspects)
(provide (all-from-out 'aspects)
         (prefix-out : (all-from-out 'aspects))  ; TODO remove these
         )

(define (run-tests)
  (define thing 'things-must-also-be-defined-beforehand)
  (define aspect 'aspects-need-to-be-defined-beforehand)
  (define-values (a1 a2 a3) (values 'a1 'a2 'a3))
  (define-values (;a at
                  atv aaaa)
    (values ;(: aspect)
     ;(: thing aspect)  ; FIXME
     (: thing aspect 'value)
     (: thing a2 ([a1 1] [a3 3]))
     ; (: thing ([a1 1] a2 [a3 3])) ; fails as expected
     ))

  (.identifier DOI: 10.1234/hello.world PMID: 1234567)
  ((.inputs a b c))
  
  (spec (*: g weigh) ;(#%measure :g weigh)
        (def bob 'hello?)
        (.invariant (< 0.01 (error g))))

  (spec (:* mass cut-down-to-size) ;(#%actualize :mass cut-down-to-size)
        (.vars final-weight))

  (spec (actualize mass cut-down-to-size)
        ; TODO there is no issue with having multiple different specifications with the same name
        ; since they all point to the same underlying object
        ; HOWEVER we probably do need to warn people that they cannot overwrite
        ; and we need to error in cases where there are conflicts
        (.vars final-weight))

  (spec (** solution) ;(#%make solution)
        (.vars final-volume)
        (.inputs solute   ; FIXME allow arbitrary order
                 ;[solvent (= solvent :volume final-volume)]
                 ;[: solvent (= volume final-volume)]
                 [: solvent ([volume final-volume])]  ; TODO asp-exp

                 ;(with-invariants [(> :volume final-volume)] beaker)
                 ;comm for testing;[beaker (> :volume final-volume)]  ; this is super nice for lisp; beaker :volume > final-volume possible
                 ;comm for testing;[beaker (> beaker :volume final-volume)]
                 ;[beaker (> (: _ volume) final-volume)]  ; FIXME how to bind beaker implicitly?
                 [: beaker ([volume final-volume])]  ; TODO asp-exp
                 ;[: beaker (volume final-volume)]  ; basic equality but in theory could extend...
                 ;(: beaker [= volume final-volume])  ; more consistent syntax also [] vs ()
                 ;(: beaker (> volume final-volume))
                 ;(: beaker [> volume final-volume])
                 ;[: beaker [> volume final-volume]]  ; <- this one!
                 ....)
        )

  (impl (solution way0)
        'no
        'steps
        'here!)
  (impl (solution way1)
        ;implicit order
        'step-0
        'step-1
        'step-2
        )
  (impl (solution way2)
        ; no dependencies
        (.order
         'another-thing-without-obvious-order
         'thing-without-obvious-order
         ))
  (impl (solution way3)
        ; with input/output dependencies (may not need)
        (.order+
         'another-thing-without-obvious-order
         'thing-without-obvious-order
         ))

  ; not at all clear why these fail before the impl sections fail...
  (displayln weigh)
  (displayln cut-down-to-size)
  (displayln solution)
  )

(module+ test
  (run-tests)
  )
