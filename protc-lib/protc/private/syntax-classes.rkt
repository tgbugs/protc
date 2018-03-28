#lang racket/base

(module prims racket/base
  (require syntax/parse (for-syntax racket/base syntax/parse))
  (provide (all-defined-out)
           (rename-out [:* actualize]
                       [*: measure]
                       [** make]))
  (define-syntax (: stx)
    "thing-aspect binding operator"
    ;"an intermediate step toward thing :aspect value is (: aspect thing value) and :aspect is (: aspect)"
    ; another goldmine https://docs.racket-lang.org/syntax/stxparse-patterns.html
    ; TODO nesting?? hard to translate thing :a1 v1 :a2 :a3 v2 with only one reference to thing
    ; lol not really (: thing ([a1 v1] a2 [a3 v2]) vs [a2]
    (syntax-parse stx
      ;[(_ thing:id ((~or [aspect:id value] lonely-aspect:id) ...))
      ;#'(list thing aspect ... value ... lonely-aspect ...)] 
      [(_ thing:id lonely-aspect:id ... ([aspect:id value] ...)); a more consistent approach
       ; TODO the context where this occurs is going to really affect how this is interpreted...
       ; so leave it as is for now
       #'(list thing lonely-aspect ... (cons aspect value) ...)]
      [(_ thing:id aspect:id value)
       #'(list thing (cons aspect value))]
      [(_ thing:id aspect:id)
       #'(list thing aspect)]))

  (define-syntax (*: stx)
    "measure-aspect binding"
    (syntax-parse stx
      [(_ measure-name:id aspect:id)
       #'(list measure-name aspect)]))

  (define-syntax (:* stx)
    "actualize-aspect binding
This is very useful for specifying a new actulization procedure and also for saying clearly
\"This is the actualization procedure that I intend to use.\"

The two ways that this could go are (:* my-actualization-procedure g) or (: my-actualization-procedure)
or even just (:* my-actualization-procedure) though that is unlikely to work in cases where there is more than
one thing that has aspect grams.
"
    (syntax-parse stx
      [(_ actualize-name:id aspect:id)
       #'(list actualize-name aspect)]))

  (define-syntax (** stx)  ; FIXME not sure we need or want this...
    "make section indicator"
    (syntax-parse stx
      [(_ make-name:id)
       #'(list make-name)]))

  (define-syntax (def stx)
    (syntax-parse stx
      [(_ name:id body:expr)
       #'(define name body)]))
  )

(require syntax/parse
         (only-in racket/class class? object?)
         'prims
         (for-template 'prims racket/base syntax/parse)
         (for-syntax 'prims racket/base syntax/parse)
         (for-meta -1 'prims (only-in racket/base quote))  ; OH MY GOD FINALLY
         "utils.rkt")
(provide (except-out (all-defined-out) -asp)
         (all-from-out 'prims))

(define-syntax-class message-struct-sc
  (pattern (-name:id body-pattern ...)
           ; FIXME may need with-syntax
           #:attr name (name-join "." #'-name this-syntax)
           #:attr sc-name (name-join "sc-" #'-name this-syntax)))
  

(define-syntax-class -asp
  ; NOTE we do not need this form anymore, it is more consistent to refer to unbound aspects without the colon
  ; so for example if I want to spec a measure for grams I would (spec (*: g)) which is much clearer
  #:description "(: aspect:id)"
  #:literals (:)
  (pattern (: aspect:id)))

(define-syntax-class asp
  #:description "(: thing:id lonely-aspect:id ... ([aspect:id value] ...))"
  #:literals (:)
  (pattern (: thing:id -aspect:id ... (~optional ([aspect-value:id value] ...)))
           ; switch order to allow the values to line up
           ;#:attr aspect-value #`(list #,(if (attribute -aspect-value) #'-aspect-value null) ...)
           #:attr aspects #'(list aspect-value ... -aspect ...)
           #:attr values #'(list value ...))
  ;(pattern (: thing:id aspect-value:id value))  ; normally only want (t a v) but (t a a a a v) is more consistent
  ;(pattern (: thing:id aspect-l:id ...))
  ;(pattern (: thing:id (~optional aspect:id ...) (~optional ([aspect-value:id value] ...))))
  ;(pattern (: thing:id aspect:id ... (~optional ([aspect-value:id value] ... ))))
  ;(pattern (: thing:id aspect:id ...))
  ;(pattern (: thing:id (~seq aspect:id value) ...))

  ; new possibility
  ;(pattern (: thing:id aspect:id (operator aspect:id value) ... ))
  ;(pattern (: thing:id aspect:id aspect-constraint:sc-asp-inv ...)) ; for single aspect only?
  ;(pattern (: thing:id [aspect-value:id sc-asp-inv] ...))  ; HRM and let*?
  )
;(pattern (: thing:id lonely-aspect:id ... ([aspect:id value] ...))))

(define-syntax-class are-you-kidding-me
  #:literals (:)
  (pattern (: (~seq aspect:id value) ...)))  ; yay for ~seq :)

(define warning-string
  "WARNING: ~a already bound! Modifying an existing spec. Duplicate starts on line ~a\n")

(define-syntax-class actualize-sc
  #:description "(:* aspect:id section-name:id)"
  #:literals (:* actualize)
  (pattern ((~or* :* actualize) aspect:id name:id)
           #:do ((when (identifier-binding #'name)
                   (printf warning-string
                           (syntax-e #'name)
                           (syntax-line #'name))))
           #:attr type #'actualize))

(define-syntax-class measure-sc
  #:description "(*: aspect:id section-name:id)"
  #:literals (*: measure)
  (pattern ((~or* *: measure) aspect:id name:id)
           #:do ((when (identifier-binding #'name)
                   (printf
                    warning-string
                    (syntax-e #'name)
                    (syntax-line #'name))))
           #:attr type #'measure))

(define-syntax-class make-sc
  ; TODO not clear what the most consistent way to do this is
  ; bind a name for a new black box VS specify how to make it???
  #:description "(** thing-name:id)"
  #:literals (** make)
  (pattern ((~or* ** make) name:id)
           #:do ((when (identifier-binding #'name)
                   (printf
                    warning-string
                    (syntax-e #'name)
                    (syntax-line #'name))))
           #:attr type #'make))

(define-syntax-class impl-sc
  #:description "(spec-name:id [impl-name:id])"
  (pattern (spec:id (~optional -name:id))  ; TODO ordering...
           ; maybe at some point revisit fail-when? (see 238ca3559)
           #:attr name (if (attribute -name) #'-name #'spec)
           #:attr type #'measure))


(define-syntax-class def-sc
  #:description "(def name:id body:expr)"
  #:literals (def)
  (pattern (def name:id body:expr)))

(define-syntax-class message-sc
  #:literals (quote def)
  (pattern doc:str
           #:attr type 'doc)
  (pattern (quote thing)
           #:attr type 'quote
           )
  (pattern l-def:def-sc
           #:attr name #'l-def.name
           #:attr body #'l-def.body
           #:attr type 'def)
  (pattern (name:id body ...)  ; #%message handles the type on body and needs ellipis
           #:attr type 'message))

(define-syntax-class impl-spec-body-sc
  (pattern
   (~or* definition:def-sc message:message-sc)

   ;(~seq def:def-sc ...)
   ;(~seq message:message-sc ...)
   ;#:attr defs #'(definition ...)
   ;#:attr messages #'(message ...)
   ))
