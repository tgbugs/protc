#lang racket/base

(module prims racket/base
  (require syntax/parse
           (for-syntax racket/base (rename-in syntax/parse [expr syntax-expr])))
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
      [(_ name:id body:syntax-expr)
       #'(define name body)]))

  (define-syntax (aspect-lifted stx)
    #`(quote #,stx))
  )

;(for-template (only-in racket/base #%top #%app quote))

(require racket/dict
         racket/list
         racket/syntax
         (rename-in syntax/parse [expr syntax-expr])
         (only-in racket/class class? object?)
         (only-in racket/string non-empty-string?)
         'prims
         (for-template 'prims racket/base syntax/parse)
         (for-syntax 'prims
                     racket/base
                     racket/list
                     (rename-in syntax/parse [expr syntax-expr])
                     protc/units/si-units-data
                     protc/units/si-units-extras
                     protc/units/units-dimensionless
                     protc/units/imperial-units-data
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

(define-syntax-class nestr
  (pattern string:str #:fail-unless (non-empty-string? (syntax-e #'string)) "empty string not allowed in this context"))

(module+ test
  (syntax-parse #'"hello" [string:nestr #t])
  (syntax-parse #'"1" [string:nestr #t])
  (check-exn exn:fail:syntax? (thunk (syntax-parse #'"" [string:nestr #t]))))

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
  #:description "(def name:id body:syntax-expr)"
  #:literals (def)
  (pattern (def name:id body:syntax-expr)))

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
                  (~optional validate/being:syntax-expr)  ; validate/being validate/being->symbol? works on both inputs and outputs and is part of define/   ; TODO this needs a generic portion to map symbols to verification funcations
                  ;(~optional require)  ; ~or* on being->symbol and symbol->being for each name, essentially (define/symbol->being mouse) -> a function that when given 'mouse returns the protocol... can be imported by name
                  (~optional (.uses imports ...))  ; reference required by name
                  ;(~optional symbol->being)  ; (define/symbol->being thing #:name my-actualization-protocol-for-thing)
                  ;(~optional being->symbol)
                  (~optional telos)
                  (~between aspect-bindings 1 +inf.0)
                  thing:str
                  ) ...
            body:syntax-expr ...
            )
           #:attr invariant-binding-form (if (attribute invariant-binding-form-nested)
                                             #'(invariant-binding-form-nested ...)
                                             #''())  ; FIXME
           #:attr inputs '()
           #:attr outputs '()
           #:attr required-symbolic-inputs '()
           #:attr required-symbolic-outputs '()
           )  ; FIXME TODO

  (pattern lone-body:syntax-expr
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
  (pattern instruction:str
           #:attr name #f
           #:attr name-fake ; name implies that it is bound as an identifier, since we don't lift strings to define them as implicit steps first things break, for now leave name empty until we can figure out a better way
           #; ; both approaches causes syntax-local-value: unbound identifier: #<syntax g239023-wat-stx> issues
           ; that are exceedingly confusing and hard to debug
           (datum->syntax #'instruction (format "~a-wat" (gensym)))
           ; this one at least will provide some context for the step name, though confusion if steps have the same text
           (fmtid "~a-step-ref" #'instruction)
           #; #; ; I love that double sexp comments like this work
           #:do ((println #'instruction))
           #;
           (datum->syntax #'instruction (format-id #'instruction "~a" (gensym)))
           #;
           (datum->syntax #f `(quote ,(gensym)))
           #;
           #'null
           #;
           #f
           #:attr [args 1] #f
           )
  (pattern (name:id args:syntax-expr ...)
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
  (pattern body:syntax-expr
           #:attr validate #'"pull this value out pf the defined body structure"
           #:attr read #'"hrm, equally problematic"
           ))

(define-syntax-class sc-protc-input
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
  (pattern thingA:syntax-expr)
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

(define-syntax (define-literal-set-from-units stx)
  ;; FIXME we kind of need a way to make these user extensible, however they still need to be formal?
  ;; I think we are likely to address this when we start working to define everything in terms of the
  ;; count aspect over some thing
  (syntax-parse stx
    [(_ name)
     (define units
       (remove-duplicates (flatten (append
                                    units-si
                                    units-imp
                                    units-extra
                                    units-extra-prefix
                                    units-dimensionless
                                    units-dimensionless-prefix))))
     #`(define-literal-set name
         #:datum-literals
         ; expand units-si to pull in additional units at runtime
         #,(datum->syntax this-syntax units) ())]))

(define-literal-set-from-units funits)

(define unit? (literal-set->predicate funits))

(define-syntax (define-literal-set-from-prefixes stx)
  ; TODO make it possible pass prefixes-si in at syntax time?
  ; i think this requires one more macro to tell racket to look for
  ; the name prefixes-si at macro compile time?
  (syntax-parse stx
    [(_ name)
     #`(define-literal-set name
         #:datum-literals
         #,(datum->syntax this-syntax (remove-duplicates (flatten prefixes-si))) ())]))

(define-literal-set-from-prefixes fprefixes)

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

(define-syntax-class sc-bool
  #:datum-literals (param:bool bool)
  (pattern ((~or* param:bool bool)
            value:boolean)
           #:attr normalized #'(bool value)
           ))
(module+ test
  (check-true (syntax? (syntax-parse #'(bool #t) [bool:sc-bool #'bool.value]))))

(define (simplify-unit expr)
  ; lots of work to do here ...
  ; convert to intermediate repr
  ; then probably feed it into the conversion function
  ; check on rosette interop to kill 2 birds with 1 stone
  ;'joules
  expr
  )

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


(define-syntax-class sc-dilution  ; TODO
  #:datum-literals (dilution param:dilution)
  (pattern ((~or* dilution param:dilution) low:integer high:integer)
           #:attr aspect #'"dilution"
           #:attr normalized #'(dilution low high)
           ))

(define-syntax-class sc-ratio ; TODO
  #:datum-literals (ratio param:ratio)
  (pattern ((~or* ratio param:ratio) numerator:integer denominator:integer)
           #:attr aspect #'"ratio"
           #:attr normalized #'(ratio numerator demoninator)
           ))

(define-syntax-class sc-quantity
  #:datum-literals (quantity param:quantity
                    fuzzy-quantity protc:fuzzy-quantity
                    dimensions param:dimensions
                    expr param:syntax-expr)
  #:local-conventions ([body syntax-expr]
                       [param-expression sc-cur-expr])
  (pattern ((~or* param:quantity quantity) (~or* value:number param-expression)
                                           (~optional (~or* unit:sc-unit unit-expr:sc-unit-expr)))
           ; FIXME we do not want units to be optional, this is a bug in param:dimensions
           #:attr aspect (datum->syntax this-syntax
                                        (symbol->string
                                         (unit->aspect
                                          (syntax->datum #'(~? unit.ident (~? unit-expr.unit null))))))
           #:attr normalized #'(quantity (~? value)
                                         (~? param-expression.oper-expr)
                                         ;(~? math-expr)
                                         (~? unit (~? unit-expr null))))
  (pattern ((~or* protc:fuzzy-quantity fuzzy-quantity) fuzzy-value:str aspect:str)
           #:attr unit #f
           #:attr unit-expr #f
           #:attr normalized #'(fuzzy-quantity fuzzy-value aspect))
  
  (pattern ((~or* dimensions param:dimensions) quant:sc-quantity ...)  ; TODO
           #:attr aspect #f  ; TODO
           #:attr unit #f  ; TODO
           #:attr unit-expr #f
           #:attr normalized #'(dimesions quant ...))
  (pattern param-expression ; TODO ensure a check param:quantity prefixed by (
           #:attr aspect #f  ; TODO
           #:attr unit #f  ; TODO
           #:attr unit-expr #f
           ;#:attr normalized #'(expr body ...)
           #:attr normalized #'param-expression.normalized
           )
  )
(module+ test
  (check-true
   (syntax-parse #'(quantity 10)
     [expr:sc-quantity #t]))
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
  (pattern (oper:id (~or* expr:sc-cur-oper num:number quant:sc-quantity) ...)))

(define-syntax-class sc-cur-expr
  #:datum-literals (expr param:syntax-expr)
  (pattern ((~or* expr param:syntax-expr) oper-expr:sc-cur-oper)
           #:attr normalized #'(expr oper-expr)))

;;; curation syntax classes 

(define-syntax-class sc-cur-term
  #:datum-literals (term) ; FIXME label should probably be keyworded
  (pattern (term curie:identifier (~or* label:string #f) #:original value:str)))

(define-syntax-class sc-cur-hyp
  #:datum-literals (hyp: quote)
  ; add additional valid prov identifiers here
  ; FIXME somehow this didn't error on (hyp: 5) instead of (hyp: '5) ???
  (pattern (hyp: (quote id)))) ; was hypothesis-urlsafe-uuid but that is super annoying to type

(define-syntax-class sc-cur-fail
  #:datum-literals (param:parse-failure parse-failure)
  (pattern ((~or* param:parse-failure parse-failure)
            (~optional (~seq #:prov prov:sc-cur-hyp))
            ; TODO figure out what to do with these fellows
            body ...)))

(define-syntax-class sc-cur-circular-link
  #:datum-literals (circular-link cycle)
  (pattern (circular-link type (cycle id-n ...))
           #:attr warning (datum->syntax this-syntax
                                         (format "WARNING: circular link ~a ~a"
                                                 (syntax->datum #'type)
                                                 (cons 'cycle (syntax->datum #'(id-n ...)))))))

(define-syntax-class sc-cur-todo
  #:datum-literals (TODO)
  (pattern (TODO text:str
                 (~optional (~seq #:prov prov:sc-cur-hyp))
                 body:syntax-expr ...)))

(define-syntax (define-sc-aspect-lift stx)
  (syntax-parse stx
    ;#:datum-literals (aspect)
    #:datum-literals (rest)
    [(_ name oper-name alt ...)
     #:with -~? (datum->syntax this-syntax '~?)
     #:with elip+ (datum->syntax this-syntax '...+)
     #'(define-syntax-class name
         #:literals (aspect-lifted)
         #:datum-literals (oper-name alt ...)
         (pattern ((~or* oper-name alt ...)
                   ; FIXME hack we need a better solution for this :/ ; it implies equality
                   ; of the string when compared to some value but that is overloaded
                   ; and we definitely need to warn at the very least (or actually make it fatal)
                   raw:str)
                  #:attr lifted #f)
         (pattern ((~or* oper-name alt ...)
                   (~or*
                    quantity:sc-quantity dil:sc-dilution dil:sc-ratio boo:sc-bool fail:sc-cur-fail)
                   (~optional (rest parse-rest elip+))
                   (~optional (~seq #:prov prov:sc-cur-hyp)))
                  #:attr lifted ;(syntax/loc this-syntax
                  #'(aspect-lifted (-~? fail
                                        (-~? boo.value  ; FIXME these have to be inside an aspect
                                             (-~? dil.aspect
                                                  (-~? quantity.aspect
                                                       (raise-syntax-error
                                                        'no-unit "quantity missing unit"
                                                        this-syntax (~? quantity))))))
                                   (-~? #:prov prov) ; FIXME this is going to break due to missing #:prov probably
                                   (oper-name 'lifted (-~? quantity.normalized
                                                           (-~?
                                                            dil.normalized
                                                            (-~? boo.normalized
                                                                 fail)))))))]))

(define-sc-aspect-lift sc-cur-parameter* parameter* protc:parameter*)

(define-sc-aspect-lift sc-cur-invariant invariant protc:invariant)

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

  (check-equal? (syntax-parse #'(parameter* (bool #t) (hyp: '0))
                  [thing:sc-cur-parameter* (syntax->datum #'thing.lifted)])
                '(aspect-lifted #t (hyp: '0) (parameter* 'lifted (bool #t))))

  (check-true
   (syntax-parse #'(parameter* (quantity 10
                                         (unit-expr (/ (unit meters)
                                                       (unit seconds))))
                               (hyp: '0))
     [thing:sc-cur-parameter* #'thing.lifted #t]))

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

;(define-sc-aspect-lift sc-cur-*measure *measure protc:*measure)  ; TODO structure is different?
; fun thing about this is that it is now very clear how to use *measure
; to let people specify the structure of the result that is going to come
; out at run time, it also fits very nicely as the mirror of parameter in this sense
(define-syntax-class sc-cur-*measure
  ; TODO way to provide more structure, but maybe not from this syntax class
  #:datum-literals (*measure protc:*measure)
  (pattern ((~or* *measure protc:*measure)
            text:str
            (~optional (~seq #:prov prov:sc-cur-hyp))
            body ...)) ; TODO 
  )

(define-syntax-class sc-cur-calculate
  ; this is actually nice to tag with because it is immediately clear
  ; that there is missing information when you say calculate the value
  ; and there is no measurement inbetween, for some specs it doesn't
  ; really matter how, but for others it does, calculated aspects without
  ; hows warn
  ; maybe name it protc:*calculate for clarity?
  ; and use protc:compute for pure symbol? not quite right ...
  ; having both forms would be confusing and probably unneeded
  #:datum-literals (calculate protc:calculate)
  (pattern ((~or* calculate protc:calculate)
            text:str
            (~optional (~seq #:prov prov:sc-cur-hyp))
            body ...)) ; TODO 
  )

(define-sc-aspect-lift sc-cur-result result protc:result)
; TODO lift results to *measure or calculate specs

(define-syntax-class sc-cur-bbc
  #:datum-literals (bbc black-box-component protc:black-box-component)
  (pattern ((~or* bbc black-box-component protc:black-box-component)
            (~or* name:nestr term:sc-cur-term)
            (~optional (~seq #:prov prov:sc-cur-hyp))
            ; TODO I think we only allow nested bbcs which are implicitly interpreted as a
            ; part-of hierarchy and I think that might be able to act as inputs to other
            ; steps? or do we not want to support that since you could specify a step that
            ; has a brain slice as an input with a parameter attached to a bbc cell-body
            ; EXAMPLE make my-special-sandwich has input bread which has-part holes and
            ; seeds, and then you could say that the cateogry aspect of the seed should be
            ; one of a list probably better to use something like (any-of seasme flax
            ; pumpkin) which would be done via a qualifier on the black box on the other
            ; hand one might also want to specify "bread with holes that are less than .5
            ; cm in diameter" with a telos "so that the jelly doesn't leak through"
            (~alt
             unconv:str
             child:sc-cur-bbc
             fail:sc-cur-fail
             asp:sc-cur-aspect
             asv:sc-cur-aspect-vary
             ; FIXME do we restrict the aspect structure here?  OR should we only
             ; allow aspects to have bbcs as context?  in other words, we need a way
             ; to know whether the bbc has a parent that is the primary input, or
             ; whether it is the black box complement (environment) TODO require
             ; non-recursive definition of bbcs in this hierarchy so that a location
             ; aspect can use previously definted components that are local to the
             ; context without risk of collision
             par:sc-cur-parameter*
             inv:sc-cur-invariant) ...))
  )

(module+ test
  (check-true
   (syntax-parse
       #`(bbc (term ilxtr:lol "lol" #:original "lul") (hyp: '-1))
     [expr:sc-cur-bbc #t]))
  ; TODO categorical values require that the measure function
  ; return only one of the set of values, so something like, or a cond
  ; (define-aspect (crust hardness) (if (hard? crust) 'hard 'soft))

  ; NOTE that in protc/base that sometimes the units can be specified outside the expression
  ; we also support units inside the expression as here so that unit conversions can be
  ; applied automatically and so that we can trace the provenance of the actual raw measures
  (check-true
   (syntax-parse
       #'(black-box-component "bread" (hyp: '-1)
                              (black-box-component "crust" (hyp: '0)
                                                   (aspect "soft?" (hyp: '1)
                                                           (parameter* (bool #t)
                                                                       (hyp: '2))))
                              (black-box-component "holes" (hyp: '3)
                                                   (aspect "diameter" (hyp: '4)
                                                           (parameter*
                                                            (expr (< (quantity 0.5 (unit 'meters 'centi))))
                                                            (hyp: '5)))))
     [expr:sc-cur-bbc #t]))
  ; NOTE nesting aspects is not something we want to do
  ; however, it does mean that we need to figure out how to
  ; express projection vectors
  (define cell-1-aspect
    ; this seems like it will require overloading of the location aspect type ...
    ; the operational definition of this would be "500um +/- 30um from the surface of the brain"
    ; which we can work since it implies that the missing how is a contextual definition for what
    ; layer 5 means and that it returns true, we also know that inside it is a spatial-3
    ; but in this case we just have it as a categorical location ... HRM I think we can manage that
    ; by lifting the type of the parameter (bool, quantity, etc.)
    #'(aspect "location" (hyp: '0)
              (context "in" (hyp: '1) (bbc "layer 5" (hyp: '0.5)))  ; "in" optional tagged this way before
              (parameter* (bool #t) (hyp: '2)))
    #; ; this is retained as an example of a bad aspect definition that does not work
    #'(aspect "location" (hyp: '1)  ; FIXME categorical aspects? no?
              (aspect "in?" (hyp: '1.5)
                      ; better to have l5 a context than allow categories?
                      ; what about classification functions that really do
                      ; return string values? well, that is some hefty context
                      ; that would probably have to be defined as a special
                      ; categorical unit or something like that?
                      (context "layer 5" (hyp: '1.6))
                      (parameter* (bool #t) ;(category "layer 5")
                                  (hyp: '2)))))
  (check-true (syntax-parse cell-1-aspect [expr:sc-cur-aspect #t]))
  (check-true
   (syntax-parse
       #`(bbc "some brain region" (hyp: '-1)
              ; implicit let* here?
              (bbc "cell 1" (hyp: 'cell-1-anno)
                   #,cell-1-aspect
                   )
              (bbc "cell 2" (hyp: '3)
                   (aspect "connected?" (hyp: '4)
                           (context (hyp: 'cell-1-anno))  ; FIXME by reference
                           (parameter* (bool #t) (hyp: '5)))))
     [expr:sc-cur-bbc #t]))

  )

(define-syntax-class sc-cur-vary
  #:datum-literals (vary protc:vary protc:implied-vary)
  (pattern  ((~or* vary protc:vary protc:implied-vary)
             name:nestr
             (~optional (~seq #:prov prov:sc-cur-hyp))
             (~or*
              (~seq unconv:str ...)
              (~seq inv:sc-cur-invariant ...)
              (~seq par:sc-cur-parameter* ...)))))

(define-syntax-class sc-cur-aspect-vary
  #:datum-literals (protc:aspect-vary)
  (pattern  ((~or* aspect-vary)
             (~or* name:nestr term:sc-cur-term)
             (~optional (~seq #:prov prov:sc-cur-hyp))
             (~or*
              (~seq unconv:str ...)
              (~seq inv:sc-cur-invariant ...)
              (~seq par:sc-cur-parameter* ...)))))

(define-syntax-class sc-cur-aspect
  #:datum-literals (aspect protc:aspect protc:implied-aspect aspect-alt)
  ; reminder, don't need unprefixed implied- because if you are unprefixed you are writing the protocl directly
  (pattern ((~or* aspect protc:aspect protc:implied-aspect aspect-alt)
            (~or* name:nestr term:sc-cur-term)
            (~optional (~seq #:prov prov:sc-cur-hyp))
            (~alt
             (~between (~or* asp:sc-cur-aspect
                             asv:sc-cur-aspect-vary
                             cnt:sc-cur-context
                             bbc:sc-cur-bbc) 0 +inf.0)
             (~once (~or* unconv:str ; FIXME this isn't actually ~once due to the ... ???
                          fail:sc-cur-fail
                          inv:sc-cur-invariant
                          par:sc-cur-parameter*
                          mes:sc-cur-*measure
                          cal:sc-cur-calculate
                          res:sc-cur-result
                          var:sc-cur-vary
                          ))) ...
            ; TODO lift bbc to context, need way to define valid context(s) in define-aspect?
            ; FIXME how the heck do you define bindings for actualizing a vector aspect?
            ;  compositionally I think it is possible to define actualization of sub-aspects
            )
           #:attr warning #f)

  (pattern ((~or* aspect protc:aspect protc:implied-aspect aspect-alt)
            (~or* name:nestr term:sc-cur-term)
            (~optional (~seq #:prov prov:sc-cur-hyp))
            (~between (~or* asp:sc-cur-aspect
                            asv:sc-cur-aspect-vary
                            cnt:sc-cur-context
                            bbc:sc-cur-bbc) 0 +inf.0) ...)
           #:attr inv #f
           #:attr par #f
           #:attr mes #f
           #:attr cal #f
           #:attr res #f
           #:attr var #f
           #:attr warning #'"Aspect has no body section!"))

(module+ test
  (check-true
   (syntax-parse #'(aspect "length"
                           (hyp: 'some-prov)
                           #; ; fails as expected
                           (parameter* (quantity 10 (unit 'meters 'milli)) (hyp: 'p0))
                           (parameter* (quantity 20 (unit 'meters 'milli)) (hyp: 'p1)))
     [thing:sc-cur-aspect #t]))

  (check-true
   (syntax-parse #'(aspect "predicate?"
                           (hyp: 'some-other-prov)
                           (parameter* (bool #t) (hyp: 'p2)))
     [thing:sc-cur-aspect #t]))

  (check-true (syntax-parse #'(aspect "dangling"
                                      (hyp: 'some-other-prov))
                [thing:sc-cur-aspect #t]))

  (define (bool value) value)  ; temp
  (define (quantity value . rest) (cons value rest))  ; temp
  (check-exn exn:fail:syntax?
             (thunk (syntax-parse #'(aspect "too many"
                                            (hyp: 'some-other-prov)
                                            (invariant (bool #t) (hyp: 'p2))
                                            (parameter* (bool #t) (hyp: 'p2))
                                            (parameter* (bool #t) (hyp: 'p3)))
                      [thing:sc-cur-aspect #t])))

  ; can we tell if an aspect is a predicate? if we can then we can
  ; automatically interpret these as the positive #t version by default
  ; can we really trust the question mark at the end though?
  ; probably for protc/base we can, maybe for protc/ur
  #; ; TODO this fails right now, enable once predicate detection happens
  (check-true
   (syntax-parse #'(aspect "predicate-default-behavior?" (hyp: 'prov))
     [thing:sc-cur-aspect #t]))
  )
(define-syntax-class sc-cur-aspect-bad
  #:datum-literals (aspect protc:aspect protc:implied-aspect aspect-alt)
  (pattern ((~or* aspect protc:aspect protc:implied-aspect aspect-alt)
            (~or* name:nestr term:sc-cur-term)
            (~optional (~seq #:prov prov:sc-cur-hyp))
            (~alt
             (~between (~or* asp:sc-cur-aspect
                             asv:sc-cur-aspect-vary
                             cnt:sc-cur-context
                             bbc:sc-cur-bbc) 0 +inf.0)
             (~between (~or unconv:str
                         inv:sc-cur-invariant
                         par:sc-cur-parameter*
                         mes:sc-cur-*measure
                         cal:sc-cur-calculate
                         res:sc-cur-result
                         var:sc-cur-vary) 0 +inf.0)) ...  ; difference is here, basically not once
            )
           ; TODO count the number of problem steps
           ))

(define-syntax-class sc-cur-context
  ; specifically aspect context
  ; the issue is that aspects can require multiple primary participants
  ; and also have multiple context participants with their own aspects
  #:datum-literals (context protc:context protc:implied-context)
  (pattern ((~or* context protc:context protc:implied-context)
            (~optional (~or* name:nestr term:sc-cur-term))
            (~optional (~seq #:prov prov:sc-cur-hyp))  ; for context can just have the link if child is input or bbc
            (~or* unconv:str
                  bbc:sc-cur-bbc
                  ; TODO determine what else can go as context and specifically whether
                  ; aspects on the same participant can be context or whether that needs
                  ; to be dealt with symbolically it seems like there might be some cases
                  ; where logically or temporally you have to know the value of one aspect
                  ; before you can determine the value of another aspect, which could be
                  ; interpreted as context rather than just ordering rules

                  ; asp:sc-cur-aspect

                  ; lifted to aspects, will uncomment when needed
                  ; inv:sc-cur-invariant
                  ; par:sc-cur-parameter*

                  ; previous results are likely to be used here
                  ; will uncomment when it is clear we need them
                  ; res:sc-cur-result
                  ) ...  ; FIXME ~optional ?
            )))

(define-syntax-class sc-cur-executor-verb
  #:datum-literals (executor-verb protc:executor-verb)
  (pattern ((~or* executor-verb protc:executor-verb)
            name:nestr
            (~optional (~seq #:prov prov:sc-cur-hyp))
            #; #; ; FIXME for now leave it at body because executor
                  ; verb binds completely random things and having it
                  ; unpack will show which ones are not property
                  ; structured i.e. because an aspect would be lifted
                  ; to top level
            (~alt
             unconv:str
             inp:sc-cur-input
             out:sc-cur-output
             ) ...
            body:syntax-expr ...)))

(define-syntax-class sc-cur-qualifiable
  (pattern (~or* inp:sc-cur-input
                 inv:sc-cur-aspect
                 exv:sc-cur-executor-verb
                 ;inv:sc-cur-invariant
                 ;par:sc-cur-parameter*
                 ;mes:sc-cur-*measure
                 ; leaving out ipm since the aspect should be lifted preior to qualifiation?
                 )))

(define-syntax-class sc-cur-any-qualifier
  ; annotations on aspects or inputs implemented as the parent node in a tree
  ; anything parent class that allows a qualifier should set the default =must=
  ; qualifier during expansion

  ; telos and friends are qualifiers that live external to a process since
  ; the process could be carried out for any number of reasons so this nesting
  ; structure makes sense

  ; TODO does protc:objective* fit here as well?
  ; does it make sense to use this as a way to couple other parameters?
  ; what if there are multiple children that all have the same reason?

  (pattern (~or* tel:sc-cur-telos man:sc-cur-many qal:sc-cur-qualifier)))

(define-syntax-class sc-cur-telos
  #:datum-literals (telos protc:telos)
  (pattern ((~or* telos protc:telos)
            text:str
            (~optional (~seq #:prov prov:sc-cur-hyp))
            child:sc-cur-qualifiable ...)))

(define-syntax-class sc-cur-many
  ; FIXME use this as syntactic sugar for constructing a population
  ; and parameterizing it? (many 10 thing) -> (bbc things (bbc thing) (count (-> things thing) 10))
  ; there is no sane way to deal with the fact that the number of times a thing appears cannot
  ; be an aspect of an individual member :/
  ; maybe (many thing 10) -> (bbc things (aspect count (bbc thing) 10)) ; also bad
  ; -> (bbc things (only (made-up-of (many (bbc thing)))) (aspect amount 10))
  ; -> (bbc things (only (made-up-of (many (bbc thing)))) (aspect number-of-members 10))
  ; it is possible to use amount/count with composite entities such as salt, or water, or spoons
  ; it doesn't work so well for mice, you need the composite population there-of which is
  ; quite different, if know that a black box is a homogenous composite then we can allow ammount
  ; as an aspect (individual->homogenous-composite thing), alternately if we _know_ that a thing
  ; is _not_ composite then we could just lift count the other way
  ; namely (input thing (aspect count (param 10)) (aspect grams (param 1)))
  ; -> (many (thing (aspect grams (param 1))) 10) which seems reasonable, but
  ; maybe we want to use (aspect count-lift 10) or something to distingish?
  ; otherwise we have to find a way to infer composite homogenous ... vs signular this can
  ; help us prevent a proliferation of plural forms unless absolutely needed obviously if
  ; you have a situation where you need 10 individuals that have a specific relational
  ; structure between them (such as multi-generation parent/offspring) you can't use many
  ; and describe everything as parameters on a single individual FIXME call this count?
  #:datum-literals (many protc:many)
  (pattern ((~or* many protc:many)
            (~or* inp:sc-cur-input bbc:sc-cur-bbc)  ; one at a time here
            (~optional (~seq #:prov prov:sc-cur-hyp))
            (~optional par:sc-cur-parameter*)
            )))

(define-syntax-class sc-cur-qualifier
  ; TODO in protc/base qualifiers are probably a subset of control flow
  #:datum-literals (qualifier protc:qualifier)
  (pattern ((~or* qualifier protc:qualifier)
            (~or* "must" "should" "when" "unless"
                  "seems to work better when"
                  "you can not do this")
            (~optional (~seq #:prov prov:sc-cur-hyp))
            child:sc-cur-qualifiable ...)))

(define-syntax-class sc-cur-output
  #:datum-literals (output protc:output protc:implied-output)
  (pattern ((~or* output protc:output protc:implied-output)
            (~or* name:nestr term:sc-cur-term)
            (~optional (~seq #:prov prov:sc-cur-hyp))
            (~alt
             unconv:str
             crc:sc-cur-circular-link  ; FIXME need to deal with this properly
             inp:sc-cur-input  ; allow nested inputs, will probably need to split spec vs impl here
             bbc:sc-cur-bbc
             obj:sc-cur-objective* ; FIXME this would be the way make this but I'm seeing the "why actualize this"
             qal:sc-cur-any-qualifier
             ;exv:sc-cur-executor-verb
             asp:sc-cur-aspect
             asv:sc-cur-aspect-vary
             inv:sc-cur-invariant
             par:sc-cur-parameter*
             tod:sc-cur-todo
             out:sc-cur-output
             fail:sc-cur-fail) ...)
           #:attr prov-id (attribute prov.id)
           #:attr term-label (attribute term.label)
           ))

(define-syntax-class sc-cur-objective*
  ; TODO
  #:datum-literals (objective* protc:objective*)
  (pattern ((~or* objective* protc:objective*)
            (~or* name:nestr term:sc-cur-term)
            (~optional (~seq #:prov prov:sc-cur-hyp))
            body:syntax-expr ...)))


(define-syntax-class sc-cur-input-instance
  ; TODO
  #:datum-literals (input-instance protc:input-instance)
  (pattern ((~or* input-instance protc:input-instance)
            (~or* name:nestr term:sc-cur-term)
            (~optional (~seq #:prov prov:sc-cur-hyp))
            body:syntax-expr ...)))

;; TODO really need a (dereference 'prov) id for things that appear more than once
(define-syntax-class sc-cur-input
  ; FIXME it remains super annoying that failure to match a syntax class acts like a json schema failure and doesn't report the deepest point reached
  #:datum-literals (input protc:input protc:implied-input) ; FIXME TODO make this work with the (prefix-in) require combinator?
  ;#:literals (input implied-input) ; grrrrrrrrrrrrrrr
  ; syntax classes enforce the literal structure, but they can't reference the actual
  ; syntax that they constrain, which is beyond annoying due to the prefix-in issues
  (pattern ((~or* input protc:input protc:implied-input)
            (~or* name:nestr term:sc-cur-term)
            (~optional (~seq #:prov prov:sc-cur-hyp))
            (~alt ;inv/par:sc-cur-inv/par
             unconv:str
             crc:sc-cur-circular-link  ; FIXME need to deal with this properly
             inp:sc-cur-input  ; allow nested inputs, will probably need to split spec vs impl here
             inpi:sc-cur-input-instance ; more like input-type in its usage here, but understandable
             obj:sc-cur-objective*
             bbc:sc-cur-bbc
             qal:sc-cur-any-qualifier
             ;exv:sc-cur-executor-verb
             asp:sc-cur-aspect
             asv:sc-cur-aspect-vary
             inv:sc-cur-invariant
             par:sc-cur-parameter*
             tod:sc-cur-todo
             fail:sc-cur-fail
             ; TODO tighter restrictions needed here
             #;body:syntax-expr) ...
            )
           #:attr prov-id (attribute prov.id)
           #:attr term-label (attribute term.label)
           #:attr (inv-lifted 1) (attribute inv.lifted)
           #:attr (par-lifted 1) (attribute par.lifted)
           ))

(module+ test
  ; TODO counting and allocation, singular vs collectivty entities number of members of a
  ; collective vs occurances of a single thing it seems that we want the behavior of count
  ; to differ depending on whether we are referring to the input or the output but this
  ; seems like it is a bad design ...
  ; (input toothpick (aspect allocation (parameter 10))) still has the issue because the
  ; input is singular and we will only define toothpick once, do we really have to force
  ; people to define box-of-toothpicks?  maybe another operation that will automatically
  ; lift an individual definition to a population, and then we can parameterize the
  ; population? have member be a function?  but then we still need a way to pluralize ...
  ; (input [toothpicks (many toothpick)] (bbc (member toothpicks) (aspect count (param 10))))
  ; local binding for for inputs? many expand to > 1 the real question is which approach
  ; is going to be the most transparent and can we prevent people from using more
  ; confusing approaches, or at least can we detect and transform equivalent ways to say
  ; the same thing into the preferred format? which one will require the least amount of
  ; writing custom functions to check that an aspect was populated correctly?

  ; I really think that count should be able to have a type ...
  ; it would make building the defining tower for all units much easier ...

  ; dealing with count ambiguity if the aspect is count then do we ever allow a being to
  ; show up in the unit technically the "count of thing" is of a different type or
  ; putatively different type, then other counts and by carrying it along as a unit we can
  ; do fun things like express 5000 sailors / 20 ships -> 250 sailors/ship which is much
  ; less ambiguous than trying to interpret (aspect count (bbc ships) (parameter 10)) vs
  ; (thing ships (aspect count (parameter 10))) which really should be (thing ships
  ; (aspect count (bbc member) (parameter 10))) basically, how do we express that I need
  ; 10 toothpicks?  how do I express to report that we counted 20 birds?
  (check-true (syntax-parse #'(parameter* (quantity 10) (hyp: '2)) [expr:sc-cur-parameter* #t]))
  (check-true
   (syntax-parse
       #'(input "thing" (hyp: '0)
                (aspect "count" (hyp: '1)
                        (parameter* (quantity 10) (hyp: '2))))
     [expr:sc-cur-input #t]))

  ; this seems like it would need to be a complex aspect
  ; has-part-count
  (check-true
   ; bad way to do it
   (syntax-parse
       #'(input "thing" (hyp: '0)
                (aspect "count" (hyp: '1)
                        (parameter* (quantity 10 #;(unit 'part-of-thing)) (hyp: '2))))
     [expr:sc-cur-input #t]))
  (check-true
   (syntax-parse
       #'(input "thing" (hyp: '0)
                (aspect "has-part-count" (hyp: '1)
                        (bbc "part of thing" (hyp: '2))
                        (parameter* (quantity 10) #;(unit 'part-of-thing) (hyp: '3))))
     [expr:sc-cur-input #t]))
  ; (count-type "part-of-thing") instead of (unit 'part-of-thing) ?
  ; this would allow smarter lifting of counts maybe and provide a shorthand
  ; for (input thing (bbc part-of-thing) (count part-of-thing 10))
  ; or something like that, since part of thing is not constrained by how many of it there are
  ; maybe we use (input thing (many (quantity 10) (bbc part-of-thing))) with many as a qualifier?
  ; this seems much more reasonable and much more compositional
  )

(define-syntax-class sc-provide-for-export
  #:datum-literals (for-export)  ; FIXME
  (pattern (for-export provide-spec ...)))

(define-conventions conv-ur-parts
  [name nestr]
  [term sc-cur-term]
  [prov sc-cur-hyp]
  [unconv str]
  [in sc-cur-input]
  [out sc-cur-output]
  [asp sc-cur-aspect]
  [asv sc-cur-aspect-vary]
  [inv sc-cur-invariant]
  [par sc-cur-parameter*]
  [bbc sc-cur-bbc]
  [crc sc-cur-circular-link]
  [ipi sc-cur-input-instance]
  [obj sc-cur-objective*]
  [qal sc-cur-any-qualifier]
  [exv sc-cur-executor-verb]
  [tod sc-cur-todo]
  [var sc-cur-vary]
  [fail sc-cur-fail]

  #; ; not implemented
  [bb sc-cur-bb]
  )
