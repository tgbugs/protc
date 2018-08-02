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

All actualize sections should specify a variable name that will be used in inheriting sections.
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

;(for-template (only-in racket/base #%top #%app quote))

(require syntax/parse
         racket/dict
         racket/syntax
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
  ; TODO get this right...
  #;
  (pattern (: thing:id -aspect:id ... ([aspect-value:id value] ...)))

  #;
  (pattern (: thing:id -aspect:id ...)  ; FIXME consumes ....
            #:attr aspects #'(list -aspect ...)
            #:attr values #'())
  #;
  (~and (~not ....))
  (pattern (: thing:id -aspect:id ... (~optional ([aspect-value:id value] ...)))
           ; switch order to allow the values to line up
           ;#:attr aspect-value #`(list #,(if (attribute -aspect-value) #'-aspect-value null) ...)
           #:attr aspects (if (attribute aspect-value)
                              #'(list aspect-value ... -aspect ...)
                              #'(list -aspect ...))
           #:attr values (if (attribute value)
                             #'(list value ...)
                             #'()))
  #;
  (pattern (: thing:id (~or* -aspect:id [-aspect-value value]) ...)
           #:attr aspects #'(list aspect-value ... -aspect ...)
           #:attr values #'(list value ...))
  #;
  (pattern (: thing:id aspect-value:id value))  ; normally only want (t a v) but (t a a a a v) is more consistent
  #;
  (pattern (: thing:id aspect-l:id ...))
  #;
  (pattern (: thing:id (~optional aspect:id ...) (~optional ([aspect-value:id value] ...))))
  #;
  (pattern (: thing:id aspect:id ... (~optional ([aspect-value:id value] ... ))))
  #;
  (pattern (: thing:id aspect:id ...))
  #;
  (pattern (: thing:id (~seq aspect:id value) ...))

  ; new possibility
  #;
  (pattern (: thing:id aspect:id (operator aspect:id value) ... ))
  #;
  (pattern (: thing:id aspect:id aspect-constraint:sc-asp-inv ...)) ; for single aspect only?
  #;
  (pattern (: thing:id [aspect-value:id sc-asp-inv] ...))  ; HRM and let*?
  #;
  (pattern (: thing:id lonely-aspect:id ... ([aspect:id value] ...)))
  )

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

(define-syntax-class sc-aspect
  ;; TODO merge with protc-lib
  ;#:datum-literals (: .invariants .uses)
  (pattern [: name:id aspect:id]))

(define sym-input 'sym-input)
(define sym-output 'sym-output)
(define-syntax-class sc-protc-body
  (pattern ((~alt (~optional (.invariants invariant-binding-form-nested ...))  ; FIXME nesting
                  (~optional parameters)
                  (~optional validate/being:expr)  ; validate/being validate/being->symbol? works on both inputs and outputs and is part of define/   ; TODO this needs a generic portion to map symbols to verification funcations
                  ;(~optional require)  ; ~or* on being->symbol and symbol->being for each name, essentially (define/symbol->being mouse) -> a function that when given 'mouse returns the protocol... can be imported by name
                  (~optional (.uses imports ...))  ; reference required by name
                  ;(~optional symbol->being)  ; (define/symbol->being thing #:name my-actualization-protocol-for-thing)
                  ;(~optional being->symbol)
                  (~optional telos)
                  (~between aspect-bindings 1 +inf.0)
                  thing:string
                  ) ...
            body:expr ...
            )
           #:attr invariant-binding-form (if (attribute invariant-binding-form-nested)
                                             #'(invariant-binding-form-nested ...)
                                             #''())  ; FIXME
           #:attr inputs '()
           #:attr outputs '()
           #:attr required-symbolic-inputs '()
           #:attr required-symbolic-outputs '()
           )  ; FIXME TODO

  (pattern lone-body:expr
           #:with TODO (datum->syntax this-syntax ''TODO)
           #:with input (datum->syntax this-syntax ''input)
           #:with (output ...) (datum->syntax this-syntax '(symbol/body-output-1 symbol/body-output-2))
           ;#:with sym-input (datum->syntax this-syntax ''sym-input)  ; why does _this_ fail!??!
           #:with sym-output (datum->syntax this-syntax 'symbol/body-symbolic-output-1)

           #:attr invariant-binding-form #'TODO
           #:attr validate #'TODO
           #:attr validate/being #'TODO
           #:attr telos #'TODO
           #:attr required-symbolic-inputs #'sym-input  ; these need to be identifiers for the consumer
           #:attr required-symbolic-outputs #'sym-output  ; TODO > 1
           #:attr inputs #'intput
           #:attr outputs #'(output ...)
           ))

(define-syntax-class sc-step-ref
  (pattern instruction:string
           #:attr name #f
           #:attr [args 1] #f)
  (pattern (name:id args:expr ...)
           #:attr instruction
           (let ([slv (syntax-local-value
                       (format-id #'name "~a-stx" (syntax-e #'name)))])
             (datum->syntax slv (apply (eval (dict-ref (syntax->datum slv) '.docstringf)) (syntax->datum #'(args ...))))
             )))

(define-syntax-class sc-being->symbol-body
  ; TODO
  (pattern body:expr
           #:attr validate #'"pull this value out pf the defined body structure"
           #:attr read #'"hrm, equally problematic"
           ))

(define-syntax-class sc-prtoc-input
  (pattern name:id)
  ;(pattern aspect:sc-aspect)
  ;(pattern (name:id aspect:sc-aspect))  ; TODO merge
  #;(pattern (name:id unit:id)))

(define-syntax-class sc-protc-output
  (pattern name:id)
  ;(pattern aspect:sc-aspect)
  ;(pattern (name:id aspect:sc-aspect))  ; TODO merge
  #;(pattern (name:id unit:id)))

(define-syntax-class sc-id+unit
  (pattern (name:id unit:id)))
