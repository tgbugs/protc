#lang racket/base

(module prims racket/base
  (require syntax/parse
           (for-syntax racket/base syntax/parse))
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

  (define-syntax (aspect-lifted stx)
    #`(quote #,stx))
  )

;(for-template (only-in racket/base #%top #%app quote))

(require racket/dict
         racket/list
         racket/syntax
         syntax/parse
         (only-in racket/class class? object?)
         'prims
         (for-template 'prims racket/base syntax/parse)
         (for-syntax 'prims
                     racket/base
                     racket/list
                     syntax/parse
                     protc/units/si-units-data
                     protc/units/si-units-extras
                     protc/units/units-dimensionless
                     protc/units/si-prefixes-data)
         (for-meta -1 'prims (only-in racket/base quote))  ; OH MY GOD FINALLY
         "identifier-functions.rkt"
         "aspects.rkt"
         "utils.rkt")
(provide (except-out (all-defined-out) -asp)
         (all-from-out 'prims))

(module+ test
  (require rackunit
           racket/function))

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
           #:attr name #'null;#f
           #:attr [args 1] #f)
  (pattern (name:id args:expr ...)
           #:attr instruction
           (let ([slv (syntax-local-value
                       (format-id #'name "~a-stx" (syntax-e #'name)))])
             #; ;FIXME not quite ready for prime time outside of spec-1
             (datum->syntax slv (apply (eval (dict-ref (syntax->datum slv) '.docstringf))
                                       (syntax->datum #'(args ...))))
             (datum->syntax slv (dict-ref (syntax->datum slv) '.docstring))
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

(define-syntax-class sc-action
  #:datum-literals (*: >^> measure :* v> actualize)
  (pattern ((~or *: >^> measure :* v> actualize) icipant (~optional aspect) (~optional more))
           #:attr (vars 1) #''TODO  ; syntax-local-value or something from the icipant
           #:attr (act-measure 1) #''TODO
           #:attr (measure 1) #''TODO
           )
  )

(define-syntax-class sc-block-type
  #:datum-literals (measure actualize make)
  (pattern (~or measure actualize make)))

(define-syntax-class lol
  (pattern thingA:expr)
  (pattern thingB:keyword))

;;; literal sets

#;
(define-syntax .inputs  ; this KILLS the current implementation of .inputs
  ; TODO syntax parameter?
  #'(ya done goofed))

(define-literal-set protc-fields
  #:datum-literals
  (.executor
   .uses
   .vars
   .inputs
   .outputs
   .constraints  ; FIXME if an entry is missing from this list the error is completely nonsense
   .config-vars
   .symret
   .steps
   .measures  ; FIXME remove archaic
   )
  (;.inputs
   ))

(define-literal-set identifier-functions
  (hyp:
   DOI:
   PMID:))

(define id-func? (literal-set->predicate identifier-functions))

(define-syntax-class sc-id-func
  (pattern (func:id value)
           #:fail-unless (id-func? #'func) "not a known identifier function"
           ))

(module+ test
  (check-true (begin (hyp: 'TEST-TOTALLY-A-THING) #t))

  (check-equal?
   (syntax-parse #'(hyp: 'test)
     (id:sc-id-func
      (syntax->datum #'id.value)))
   ''test)
  )

;;; units syntax classes
; ideally these should be drawn from aspect definitions at syntax time

(define-syntax (flerp stx)
  (syntax-parse stx
    [(_ name)
     (define units
       (remove-duplicates (flatten (append
                                    units-si
                                    units-extra
                                    units-extra-prefix
                                    units-dimensionless
                                    units-dimensionless-prefix))))
     #`(define-literal-set name
         #:datum-literals
         ; expand units-si to pull in additional units at runtime
         #,(datum->syntax this-syntax units) ())]))

(flerp funits)

(define unit? (literal-set->predicate funits))

(define-syntax (derp stx)
  (syntax-parse stx
    [(_ name)
     #`(define-literal-set name
         #:datum-literals
         #,(datum->syntax this-syntax (remove-duplicates (flatten prefixes-si))) ())]))

(derp fprefixes)

(define prefix? (literal-set->predicate fprefixes))

(define-syntax-class symbol
  (pattern thing
           #:do ((define sl (syntax->list #'thing)))
           #:fail-unless ; this is dumb :/
           (and sl (= 2 (length sl)) (identifier? (cadr sl))) "not a symbol"
           #:attr ident (datum->syntax this-syntax (cadr sl))))

(module+ test (check-true (syntax-parse #'(quote wat) [thing:symbol #t])))

(define-syntax-class sc-unit-name
  (pattern ident:id #:fail-unless (unit? #'ident) "not a unit")
  (pattern symb:symbol
           #:fail-unless (unit? #'symb.ident) "not a unit"
           #:attr ident #'symb.ident))

(module+ test
  (check-true (syntax-parse #''m [unit:sc-unit-name #t]))
  (check-true (syntax-parse #''meters [unit:sc-unit-name #t]))

  (check-true (syntax-parse #'m [unit:sc-unit-name #t]))
  (check-true (syntax-parse #'meters [unit:sc-unit-name #t]))
  (check-exn exn:fail:syntax? (thunk (syntax-parse #'wendigo [unit:sc-unit-name #t]))))

(define-syntax-class sc-prefix-name
  (pattern ident:id #:fail-unless (prefix? #'ident) "not a prefix")
  (pattern symb:symbol
           #:fail-unless
           (prefix? #'symb.ident) "not a prefix"
           #:attr ident #'symb.ident))
(module+ test
  (check-true (syntax-parse #''m [prefix:sc-prefix-name #t]))
  (check-true (syntax-parse #''milli [prefix:sc-prefix-name #t]))

  (check-true (syntax-parse #'m [prefix:sc-prefix-name #t]))
  (check-true (syntax-parse #'milli [prefix:sc-prefix-name #t]))
  (check-exn exn:fail:syntax? (thunk (syntax-parse #'wendigo [prefix:sc-prefix-name #t]))))

(define-syntax-class sc-unit
  (pattern (_ unit:sc-unit-name (~optional prefix:sc-prefix-name))
           #:attr ident #'unit.ident))
(module+ test
  (check-true (syntax-parse #'(unit 'meters 'milli) [expr:sc-unit #t]))
  (check-true (syntax-parse #'(unit meters milli) [expr:sc-unit #t]))
  (check-true (syntax-parse #'(param:prefix-unit 'pH) [expr:sc-unit #t]))

  ;(protc:parameter* (param:quantity 7.4 (param:prefix-unit 'pH)) (hyp: 'QaiHMm5kEee7iSNIQtkMfg))
  )

(define-syntax-class sc-dim-unit
  #:datum-literals (^)
  (pattern (^ unit:sc-unit dim:integer)))

(define-syntax-class sc-unit-oper
  (pattern solo-dunit:sc-dim-unit)
  (pattern (oper:id (~or* expr:sc-unit-oper unit:sc-unit dunit:sc-dim-unit) ...)))
(module+ test
  (check-true (syntax? (syntax-parse #'(* (unit meters milli)) [expr:sc-unit-oper #'expr])))
  (check-true (syntax? (syntax-parse #'(* (unit siemens mega)
                                          (^ (unit seconds) -1))
                         [expr:sc-unit-oper #'expr])))
  (check-exn exn:fail:syntax? (thunk (syntax-parse #'(* 10
                                                        (unit meters milli)
                                                        (^ (unit seconds)))
                                       [expr:sc-unit-oper #'expr]))))

(define (simplify-unit expr)
  ; lots of work to do here ...
  ; convert to intermediate repr
  ; then probably feed it into the conversion function
  ; check on rosette interop to kill 2 birds with 1 stone
  'joules)

(define-syntax-class sc-unit-expr
  #:datum-literals (param:unit-expr unit-expr)
  ; TODO do the transformation here
  ;(pattern (_ unit:sc-unit-name (~optional prefix:sc-prefix-name)))
  (pattern ((~or* param:unit-expr unit-expr) unit-oper:sc-unit-oper)
           #:attr unit (datum->syntax this-syntax (simplify-unit #'unit-oper))
           ))
(module+ test
  (check-exn exn:fail:syntax? (thunk (syntax-parse #'(unit-expr (unit meters milli)) [expr:sc-unit-expr #'expr])))
  (check-true (syntax? (syntax-parse #'(unit-expr (* (unit meters milli))) [expr:sc-unit-expr #'expr])))
  (check-true (syntax? (syntax-parse #'(unit-expr (+ (unit meters milli))) [expr:sc-unit-expr #'expr.unit-oper])))
  (check-true (syntax? (syntax-parse #'(unit-expr (/ (unit meters) (unit seconds)))
                         [expr:sc-unit-expr #'expr.unit-oper]))))

(define-syntax-class sc-quantity
  #:datum-literals (quantity param:quantity
                             fuzzy-quantity protc:fuzzy-quantity
                             dimensions param:dimensions
                             expr param:expr
                             )
  #:local-conventions ([body expr])
  (pattern ((~or* param:quantity quantity) value:number (~optional (~or* unit:sc-unit unit-expr:sc-unit-expr)))
           ; FIXME we do not want units to be optional, this is a bug in param:dimensions
           #:attr aspect (datum->syntax this-syntax
                                        (symbol->string
                                         (unit->aspect
                                          (syntax->datum #'(~? unit.ident (~? unit-expr.unit null))))))
           #:attr normalized #'(quantity value (~? unit (~? unit-expr null))))
  (pattern ((~or* protc:fuzzy-quantity fuzzy-quantity) fuzzy-value:str aspect:str)
           #:attr unit #f
           #:attr unit-expr #f
           #:attr normalized #'(fuzzy-quantity fuzzy-value aspect))
  (pattern ((~or* dimensions param:dimensions) quant:sc-quantity ...)  ; TODO
           #:attr aspect #f  ; TODO
           #:attr unit #f  ; TODO
           #:attr unit-expr #f
           #:attr normalized #'(dimesions quant ...))
  (pattern ((~or* expr param:expr) body ...)  ; TODO
           #:attr aspect #f  ; TODO
           #:attr unit #f  ; TODO
           #:attr unit-expr #f
           #:attr normalized #'(expr body ...))
  )
(module+ test
  (check-true
   (syntax-parse #'(quantity 10 (unit seconds))
     [expr:sc-quantity #t]))
  (check-true
   (syntax-parse #'(quantity 10 (unit-expr (/ (unit meters) (unit seconds))))
     [expr:sc-quantity #t]))
  (check-true
   (syntax-parse #'(fuzzy-quantity "overnight" "duration")
     [expr:sc-quantity #t])))

(define-syntax-class sc-cur-oper
  (pattern (oper:id (~or* expr:sc-cur-oper quant:sc-quantity) ...)))

(define-syntax-class sc-cur-expr
  #:datum-literals (param:expr expr)
  (pattern ((~or* expr param:expr) oper-expr:sc-cur-oper)))


;;; curation syntax classes 

(define-syntax (define-sc-aspect-lift stx)
  (syntax-parse stx
    ;#:datum-literals (aspect)
    [(_ name oper-name alt ...)
     #:with -~? (datum->syntax this-syntax '~?)
     #'(define-syntax-class name
         #:literals (aspect-lifted)
         #:datum-literals (oper-name alt ...)
         (pattern ((~or* oper-name alt ...) quantity:sc-quantity (~optional rest) prov)
                  #:attr lifted ;(syntax/loc this-syntax
                  #'(aspect-lifted (-~? quantity.aspect
                                      (raise-syntax-error
                                       'no-unit "quantity missing unit"
                                       this-syntax quantity))
                                 prov
                                 (oper-name 'lifted quantity.normalized))))]))

(define-sc-aspect-lift sc-cur-parameter* parameter* protc:parameter*)

(define-sc-aspect-lift sc-cur-invariant invariant protc:invariant)

(define-syntax-class sc-cur-bbc
  #:datum-literals (black-box-component protc:black-box-component)
  (pattern ((~or black-box-component protc:black-box-component) body ...)) ; TODO
  )

(define-syntax-class sc-cur-aspect
  #:datum-literals (aspect protc:aspect protc:implied-aspect)
  (pattern ((~or* aspect protc:aspect protc:implied-aspect)
            name:str prov (~or* inv:sc-cur-invariant par:sc-cur-parameter*))))

(define-syntax-class sc-cur-input
  #:datum-literals (input protc:input protc:implied-input)
  (pattern ((~or* input protc:input protc:implied-input)
            name:str prov body:expr ...)))

(module+ test
  ; FIXME this works for a datum but fails hard when syntax occurs
  (check-equal? (syntax-parse #'(invariant (quantity 10 (unit meters)) (hyp: '0))
                  [thing:sc-cur-invariant (syntax->datum #'thing.lifted)])
                '(aspect-lifted "length" (hyp: '0) (invariant 'lifted (quantity 10 (unit meters)))))

  (check-equal? 
   (syntax-parse #'(invariant (fuzzy-quantity "room temperature" "temperature") (hyp: '0.5))
     [thing:sc-cur-invariant (syntax->datum #'thing.lifted)])
   '(aspect-lifted "temperature" (hyp: '0.5) (invariant 'lifted (fuzzy-quantity "room temperature" "temperature"))))

  (check-equal? (syntax-parse #'(parameter* (quantity 10 (unit meters)) (hyp: '0))
                  [thing:sc-cur-parameter* (syntax->datum #'thing.lifted)])
                '(aspect-lifted "length" (hyp: '0) (parameter* 'lifted (quantity 10 (unit meters)))))

  (syntax-parse #'(parameter* (quantity 10
                                        (unit-expr (/ (unit meters)
                                                      (unit seconds))))
                              (hyp: '0))
    [thing:sc-cur-parameter* #'thing.lifted])

  (define-syntax (testthing stx)
    ; ~? testing
    (syntax-parse stx
      [(_ (~optional a)
          (~optional (~seq #:b b))
          (~optional (~seq #:c c))
          )
       #'(~? a (~? b (~? c "d")))
       #; ; well, this works other than I thought it did
       #'(~? a b c "d")])))




