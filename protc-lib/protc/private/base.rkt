#lang racket/base
(require debug/repl (for-syntax debug/repl
                     racket/pretty
                                ))
 
(require (for-syntax racket/base
                     ;racket/class
                     syntax/parse)
         racket/class
         racket/syntax
         syntax/parse
         racket/dict)

(require (for-meta 0 racket/base)
         (for-meta 1 racket/base)
         (for-meta 2 racket/base)
         (for-meta 3 racket/base)
         (for-meta 4 racket/base)
         (for-meta 5 racket/base)) 

;;; implementation of protc forms using only racket syntax tools

;; new syntax classes that we are defining for protc
; used to enforce section ordering and structure during pattern matching
; see https://docs.racket-lang.org/syntax/stxparse-specifying.html#(part._stxparse-attrs)

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

(module utility racket/base
    ; TODO this is not working yet
  (require (for-syntax racket/base syntax/parse))
  (define-syntax (simple-syntax outer-stx)
    (syntax-parse outer-stx
      [(simple-syntax (name:id stx-expr) body:expr)
       #'(define-syntax (name stx)
           (syntax-parse stx
             [(stx-expr)
              body]
             ))]))
  (provide simple-syntax))

;(require 'utility (for-syntax 'utility))

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

(define-syntax (define-message outer-stx)
  (syntax-parse outer-stx
    [(define-message (name:id spec) body:expr)  ; TODO spec:message-spec
     #'(define-syntax (.name stx)  ; may need to use syntax->datum to add the .
         (syntax-parse stx ;#:literals (spec)
           [(_ spec)
            #'()]))]))

; *aspect says I'm going to measure this (measure aspect)
; aspect* says I'm going to actualize this (actualize aspect)
; aspect says I'm going to define this based on other aspects (define aspect)
;  or define it as a fundamental quantity aspect, id something like the kilogram
;  all other actual measurement devices used in *aspect sections

(define-syntax-class section-header
  (pattern (_?-or-aspect-name ) ))

(define-syntax (if-defined stx)
  (syntax-case stx ()
    [(_ id iftrue iffalse)
     (let ([where (identifier-binding #'id)])
       (if where #'iftrue #'iffalse))]))

(struct step (name [spec #:mutable] [impl #:mutable]) #:inspector #f); #:mutable #t)

(define step%
  (class object%
    (init name)
    (define -name name)
    (super-new)
    (define -vars '())
    (define -inputs '())
    (define -invariants '())
    (define -parameters '())
    (define -outputs '())
    (define/public (.name) -name)
    (define/public (.identifier id) id)
    ;(define/public (.input name [aspect null] [value null]) -name)  ; atomic structure of the message? ick
    (define/public (.input input-struct) (void))  ; atomic structure of the message? ick
    (define/public (.ping) 'pong)
    (define/public (.echo message) `(echoing ,message))
    (define/public (.add-spec spec)
      spec)
    (define/public (add-impl impl)
      ; validate in the context of the existing spec ??
      impl)))

(define new-step (new step% [name 'new-step]))

; placeholders to keep syntax-parse happy
(define-syntax (#%measure stx) #'(void))  ; aspect name

(define-syntax (#%actualize stx) #'(void))  ; aspect name 

(define-syntax (#%make stx) #'(void))  ; name

;(define-syntax (#%quote stx) #'ping)

(define-for-syntax (pw stx)
  (display "syntax: ") (pretty-write (syntax->datum stx)))

; message preprocessing boom extensiblity
(module message-proc racket/base
  (require (for-syntax racket/pretty racket/base syntax/parse))
  (provide (all-defined-out))
  ;(provide #%.vars
           ;#%.inputs
           ;#%.invariant
           ;(for-meta 1 #%.vars
                       ;#%.inputs
                       ;#%.invariant
           ;)
  ;)
  (define-for-syntax (pw stx)
    (display "syntax: ") (pretty-write (syntax->datum stx)))

  (define-syntax (#%.vars stx)
    (pw stx)
    #''vars
    (syntax-parse stx
      [(#%.vars v:id ...)
       #'(quote (v ...))]))
  (define-syntax (#%.inputs stx) (pw stx) #''inputs)
  (define-syntax (#%.invariant stx) (pw stx) #''invariants)
)
;(require (for-syntax 'message-proc) 'message-proc)
(require 'message-proc 
         ;(for-syntax 'message-proc)
         ;(for-meta 0 'message-proc)
         (for-meta 1 'message-proc)  ; needed for the syntax level to work
         (for-meta 2 'message-proc)  ; needed for the second level...
         (for-meta 3 'message-proc)
         (for-meta 4 'message-proc)
         (for-meta 5 'message-proc))
(println (#%.vars in begin-for-syntax))
(begin-for-syntax
  (println (#%.vars in begin-for-syntax)))
(begin-for-syntax
  (begin-for-syntax
    (println (#%.vars in lol wut))))
(begin-for-syntax
  (begin-for-syntax
    (begin-for-syntax
      (println (#%.vars in lol wut)))))
(begin-for-syntax
  (begin-for-syntax
    (begin-for-syntax
      (begin-for-syntax
        (println (#%.vars in lol wut))))))
(begin-for-syntax
  (begin-for-syntax
    (begin-for-syntax
      (begin-for-syntax
        (begin-for-syntax
          (println (#%.vars in lol wut)))))))

(define ms% (new step% [name 'ms]))
(#%.vars wtf)
(displayln "HELP ME I HAVE GONE INSANE")
(displayln (#%.vars a b c d))
(send ms% echo (#%.vars hello))
;(send* (is) (fun))
;send+ is fun too...
(let ([evaled (#%.vars a b c d)])
  (send ms% echo evaled))

(define-syntax (#%message stx)
  (pw stx)
  (syntax-parse stx  #:literals (quote)
    [(_ target (quote (dead ...))) #'(void)]  ; skip quoted stuff
    [(_ target (message-name:id message-body:expr ...))
     ; this will throw weird errors if a quoted expression is in the body... probably need to fix
     (let ([output
            (with-syntax ([process-message (name-join "#%" #'message-name stx)])
              #`(send target message-name (process-message message-body ...)))])
       (pw output)
       output)
     ;#'(void)
     ;#'(send target (message-name message-body))
     ]))

;(debug-repl)
;(begin-for-syntax (debug-repl))

(define-for-syntax (name-join prefix suffix stx)
  (datum->syntax stx  ; if this is false then everything becomes unrooted and I go looking through 5 phase levels to find the problem
   (string->symbol
    (string-append prefix (symbol->string
                           (syntax->datum suffix))))))

(define-syntax (spec stx)
    ; (spec (measure (aspect g) weigh) ...)
  (syntax-parse stx #:literals (spec  ; note that syntax-case doesn't work with syntax classes :( so body:expr fails :/
                                #%measure
                                #%actualize
                                #%make)
                [(_ (#%measure aspect:id name:id) body:expr ...)  ; TODO aspect-name:aspect
                 #'(define name (list 'aspect 'measure body ...))]
                [(_ (#%actualize aspect:id name:id) body:expr ...)  ; TODO body:messages
                 #'(define name (list 'aspect 'actualize body ...))]
                [(_ (#%make name:id) message:expr ...)
                 #'(if-defined name
                               ;(set-step-spec! name% (list 'being (#%message name% message) ...))
                               (begin (#%message name% message) ...)
                               ;(define name (step 'name (list 'being body ...) '())))]))
                               (begin
                                 (define name (step% name))  ; TODO append % to name -> name% automatically...
                                 (#%message name message) ...))]))

(define-syntax (impl stx)
  (syntax-parse stx #:literals (impl
                                #%measure
                                #%actualize
                                #%make)
                [(_ (#%measure aspect:id name:id) body:expr ...)
                 #'(define name (list 'aspect 'measure body ...))]
                [(_ (#%actualize aspect:id name:id) body:expr ...)
                 #'(define name (list 'aspect 'actualize body ...))]
                [(_ (#%make name:id) body:expr ...)
                 #'(if-defined name
                               ; TODO this is an improvement, but we really need to enforce spec first
                               ; because the impl has to look up all the terms from the spec
                               ; could look into using namespaces? how about modules... :D
                               (set-step-impl! name (list 'being body ...))
                               (define name (step 'name '() (list 'being body ...))))]))

(define-syntax (delegate stx)
  "delegate this particular term/block to the current executor"
  #'(void))

; the thing is that this doesn't work because we don't know ahead of time
; how many bound or unbound identifiers there are...
'(let-values ([() ()])
  (let ([spec-id-1]
        [spec-id-2])
    (let ([impl-id-1]
          [impl-id-2])
      (let))))

;#'(let ([rec (list 'being body ...)])
;(if-defined name
;(set-step-spec! rec)
;(step 'name rec '())))]))

;#'(define name (let ([rec (list 'being body ...)])
;(with-handlers ([exn? (λ (exn) (set-step-impl! rec))])
;(step 'name '() rec))))]))

(define-syntax (lexically-bound? stx)
  (define expanded (local-expand stx (syntax-local-context) #f))
  (and (identifier? expanded)
       (not (eq? #f (identifier-binding expanded)))))

(struct aspect (shortname name def parent)  ; note that #:auto is global...
  ; aka measurable
  #:inspector #f)

;(define unit (aspect 'unit 'unit "Units are not aspects but they can be used as aspects"))  ; units are not aspects their names can be...
; TODO define all these using (define-aspect)
(define :fq (aspect 'fq 'fundamental-quantity "The root for all fundamental quantities" 'root))

(define :count (aspect 'count 'count "How many?" :fq))
(define :mass (aspect 'mass 'mass "The m in e = mc^2" :fq))
(define :energy (aspect 'energy 'energy "hoh boy" :fq))  ; TODO synonyms... distance...
(define :length (aspect 'length 'length "hoh boy" :fq))
(define :time (aspect 'time 'time "tick tock" :fq))
(define :temperature (aspect 'temp 'temperature "hot cold" :fq))
(define :charge (aspect 'Q 'charge "hoh boy" :fq))  ; why is it current??? http://amasci.com/miscon/fund.html

(define :dq (aspect 'dq 'derived-quantity "A quantity derived from some other quantity" 'root))
(define :current (aspect 'I 'current '(/ :charge :time) :dq))  ; TODO expand quoted definitions
(define :weight (aspect 'weight 'weight "hrm..." :mass))
(define :distance (aspect 'distance 'distance "hrm..." :length))

(define :area (aspect 'area 'area '(expt :length 2) :dq))
(define :volume (aspect 'vol 'volume '(expt :length 3) :dq))

(define :mol (aspect 'mol 'mole "HRM" :count))
(define :l (aspect 'l 'liters "SI unit of weight" :volume))
(define :g (aspect 'g 'grams "SI unit of weight" :mass))

;(define :g '(aspect grams))  ; TODO
;(define :g ':g)

(spec (#%measure :g weigh)
      '(step one))
(spec (#%actualize :mass cut-down-to-size)
      '(.vars final-weight))
(spec (#%make solution)
      (.vars final-volume)
      (.inputs [solute (= solute :volume final-volume)]
               ;(with-invariants [(> :volume final-volume)] beaker)
               [beaker (> :volume final-volume)]  ; this is super nice for lisp; beaker :volume > final-volume possible
               [beaker (> beaker :volume final-volume)])
      )
(impl (#%make solution)
      'test
      'test2)

(displayln weigh)
(displayln cut-down-to-size)
(displayln solution)

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
         #:aspect :mass
         #:uses weigh
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
                    (cond ((eq? type 'being) (values being-index being-counter being-records))
                          ((eq? type 'aspect) (values aspect-index aspect-counter aspect-records))
                          (#t (error "type unkown")))])
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


(module+ test
  (displayln 'hello))
