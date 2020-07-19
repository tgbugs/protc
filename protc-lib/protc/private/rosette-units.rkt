#lang rosette
(require racket/pretty
         racket/trace
         syntax/parse
         protc/units/si-prefixes-exp-data
         protc/units/si-prefixes-data
         ;(only-in protc/private/base name-join)  ; TODO when emacs is not slow
         (for-syntax syntax/parse)
         (prefix-in racket: (only-in racket/base number?)))
; pack the invariants into into a rosette module and export the values?
; will lead to some nasty debugging exercises

(current-bitwidth #f)
;(current-bitwidth 10)  ; slooowww

(module units-syntax-classes racket/base
  (require racket/string syntax/parse
           (for-template racket/base syntax/parse))
  (provide (all-defined-out))

  (define (split-prefix-unit stx)
    ;(println stx)
    (define p-u (symbol->string (syntax->datum stx)))
    (define len (string-length p-u))
    (define fixed-stx (if (eq? #f stx) #'#f stx))
    (let-values ([(p v) (cond [(= len 1) (values #'null fixed-stx)]
          [(= len 2)
           ; FIXME TODO this fails for cases where the names are not a single char!
           (values
            (datum->syntax fixed-stx (string->symbol (substring p-u 0 1)))
            (datum->syntax fixed-stx (string->symbol (substring p-u 1 2))))]
          [(= len 3) (cond [(string-prefix? p-u "da") (error)] ; TODO enable for other 2 part char prefixes
                           [#t (error)]  ; TODO
                           )])])
      ;(println `(p-v: ,p ,v))
      (values p v)))

  (define-syntax-class maybe-prefix-unit
    (pattern prefix+unit:id
             #:attr prefix-unit (let-values ([(p u) (split-prefix-unit #'prefix+unit)]) `(,p ,u))
             #:attr prefix (car (attribute prefix-unit))
             #:attr unit (cadr (attribute prefix-unit))
             #:attr quoted-prefix #`(quote #,(attribute prefix))
             #:attr keyword (datum->syntax this-syntax (string->keyword (symbol->string (syntax->datum (attribute unit)))))
             ;#:attr kw-unit #`(~seq ,(attribute keyword) ,(attribute unit))
             )))

(require 'units-syntax-classes (for-syntax 'units-syntax-classes))

(define (new-symbol) (define-symbolic* x real?) x)

(define (milli-symbol) (define-symbolic* milli- real?) milli-)

(define (milli symbol)
  ;(ms (string->symbol (string-append "milli-" (symbol->string symbol))))
  (define new-symbol (milli-symbol))
  (assert (= new-symbol (* symbol 1000)))
  new-symbol)

(define (kilo symbol)
  (let ([n-k (new-symbol)])
    (assert (= n-k (/ symbol 1000)))
    n-k))

(define-for-syntax (name-join prefix suffix stx)
  ; from protc/base/private
  (datum->syntax stx  ; if this is false then everything becomes unrooted and I go looking through 5 phase levels to find the problem
   (string->symbol
    (string-append (if (string? prefix)
                       prefix
                       (error (format "what is this thing?! ~a" prefix)))
                   (symbol->string
                    (syntax->datum suffix))))))

(define prefix-multipliers
  (for/hash ([(name exp) (in-dict prefixes-si-exponents)])
    (values name (expt 10 (- exp)))))

(define (get-multiplier si-prefix)
  (dict-ref prefix-multipliers (dict-ref prefixes-si si-prefix)))

(define-syntax (sym stx)
  (syntax-parse stx
    [(_ name:id)
     ; surely there is a better way...
     #'((λ () (define-symbolic name real?) name))]))
  
`(test: kilo multiplier ,(get-multiplier 'k))

(define-syntax (assert-unit stx)
  (pretty-print (syntax->datum stx))
  (syntax-parse stx
    [(_ prefix+unit:maybe-prefix-unit)
     (let ([out #'(assert (= prefix+unit (* prefix+unit.unit (get-multiplier prefix+unit.quoted-prefix))))])
       (pretty-print (syntax->datum out))
       out)]))

(define-syntax (prefix-assert stx)
  (syntax-parse stx
    [(_ si-prefix:id name:id)  ; name should already be bound
     (with-syntax ([prefixed-name (name-join (symbol->string (syntax->datum #'si-prefix))
                                             #'name stx)]
                   [quoted-prefix #`(quote #,#'si-prefix)])
       (pretty-print (syntax->datum #'quoted-prefix))
       (pretty-print (syntax->datum #'prefixed-name))
       #'((λ () (define-symbolic prefixed-name real?)
             (assert (= prefixed-name (* name (get-multiplier quoted-prefix))))
             prefixed-name)))]))

(define-syntax (prefix stx)
  (syntax-parse stx
    [(_ si-prefix:id name:id)  ; name should already be bound
     (with-syntax ([prefixed-name (name-join (symbol->string (syntax->datum #'si-prefix))
                                             #'name stx)]
                   [quoted-prefix #`(quote #,#'si-prefix)])
       (pretty-print (syntax->datum #'quoted-prefix))
       (pretty-print (syntax->datum #'prefixed-name))
       #'(define prefix-name
           ; TODO replace with prefix-assert
           ((λ ()
              (define-symbolic prefixed-name real?)
              (assert (= prefixed-name (* name (get-multiplier quoted-prefix))))
              prefixed-name)
            )))]))

(define-syntax (-prefix stx)
  (syntax-parse stx
    [(_ si-prefix:id name:id)  ; name should already be bound
     ; YOU REQUIRE ADDITIONAL LAMBDAS
     (with-syntax ([prefixed-name (name-join (symbol->string (syntax->datum #'si-prefix))
                                             #'name stx)]
                   [quoted-prefix #`(quote #,#'si-prefix)])
       (pretty-print (syntax->datum #'quoted-prefix))
       (pretty-print (syntax->datum #'prefixed-name))
       #'((λ ()
            (define-symbolic prefixed-name real?)
            (assert (= prefixed-name (* name (get-multiplier quoted-prefix))))
            prefixed-name)))]))

(module+ other
;(define-symbolic M real?)
(define-symbolic g real?)
;(define-symbolic L real?)
(define-symbolic g/L real?)
;(define-symbolic g/mol real?)

(define M .01)
(define L 2)
(define g/mol 58.4)
(define NaCl:g/mol 58.4)

(assert (> M 0))
(assert (= g/L (/ g L)))

(define mM (milli M))
;(assert (> mM 0))

(solve (assert (= M (/ (/ g L) g/mol))))  ; solve includes every assert called prior to its invocation

;(define solution (synthesize #:guarantee (assert (> M 0))))
)

(define-symbolic stock:volume real?)
(define final-volume 1)
(define solution:M .120)
(define stock:M 1)
(define-symbolic stock:g stock:L real?)
(define stock:g/mol 110.9840)

(assert (= stock:M (/ (/ stock:g stock:L) stock:g/mol)))

(define thing (solve (assert (= (* solution:M final-volume) (* stock:M stock:volume)))))

(model thing)  ; how to access the results
(exact->inexact (dict-ref (model thing) stock:volume))

(define (knowns)
  ; sadly this doesn't quite work because to store all
  ; the knowns in one place we would have to have all the
  ; symbols as not-free-variables :(
  ; there may be ways around this
  ; woo combinatorial explosions
  (define (g-func g [mass 0])
    (assert (> g 0)))
  (define (m-func m [length 0])
    (assert (> m 0)))

  (define knowns-functions
    (hash `((g . ,g-func)
            (m . ,m-func))))
  (define (by-aspect aspect-name)
    (hash-ref knowns-functions aspect-name))

  by-aspect)

(define (prefix-conversions) 'TODO )  ; should probably be define-syntax

(define (prefix-convert prefix value)
  (* (get-multiplier prefix) value))

(define (all-knowns
         ; singluar
         ; all units not appearing in this film should be set to 1 by default to help the solver
         ; or maybe this doesn't help the solver?
         #:g [g (sym g)] #:m [m (sym m)] #:s [s (sym s)]
         #:L [L (sym L)] #:mol [mol (sym mol)] #:M [M (sym M)]
         #:N [N (sym N)]
         ; compound  TODO we need a way to compute these on the fly...
         #:g/mol [g/mol (sym g/mol)] #:mol/L [mol/L (sym mol/L)] #:g/L [g/L (sym g/L)]
         ; FIXME TODO should not have to do these manually
         ; there are some fun functions for setting keywords and arity...
         #:kg [kg null]
         )

  ; this block reflects my ignorance of the exact/inexact issues
  ; this works from a technical standpoint, but the issues with
  ; the ambiguity between the grams in g/mol and g/L makes it... less helpful
  ; becasue the g is not actually a _unit_ equivalence
  ; there may be a way to do that without define-symbol*
  ; this happens because the units are being solved for

  (if (null? kg)
      (set! kg (-prefix k g))
      (assert-unit kg))

  ;(pretty-print (asserts))
  (prefix m M)

  (define-symbolic anything real?)
  (assert (= anything (or g L M mol s N)))

  (assert (<= 0 g))
  ;(define kg (kilo g))  ; TODO automate this somehow
  (assert (<= 0 L))
  (assert (<= 0 M))
  (assert (<= 0 mol))  ; might conflict with the mol/L

  ; FIXME these cannot always be included or they cause failure
  (assert (= M (/ mol L)))  ; mol is ambiguous when it comes from mol/L or g/mol etc
  (assert (= M mol/L))

  ; compound
  (assert (= g/mol (/ g mol)))  ; g and mol are ambiguous
  (assert (<= 0 g/mol))
  ; FIXME TODO should be possible to infer that when going for M
  ; you can get there knowing only g, L, and g/mol, as well as other valid combinations
  ; based only on M -> mol/L and anything/L + anything/mol
  (assert (= g/L (/ g L)))  ; g is ambiguous

  (assert (= M (/ (/ g L) g/mol)))
  ;(pretty-print (asserts))

  (assert (= N (/ (* kg m) (* s s))))
  (void))

(define ? #'DEADBEEF)

; FIXME
'(if (not (eq? #f value))
     (prefix-convert unit.prefix value)
     (begin (when (not (null? unit.prefix))
              (prefix-assert unit.prefix unit.unit))
            (sym unit.unit)))

(define-syntax (let-quants stx)
  (pretty-print `(let-quants: ,stx))
  (syntax-parse stx #:literals (? prefix-convert sym)
    [(_ (symbolic-unit:maybe-prefix-unit ... [unit:maybe-prefix-unit value:expr] ... ))
     ; do the unit conversion outside the solver
     ;(pretty-print #'((~seq symbolic-unit.keyword (sym symbolic-unit.unit)) ... (~seq unit.keyword value) ... ))
     ;(pretty-print #'((symbolic-unit.keyword (sym symbolic-unit.unit)) ... (~seq unit.keyword value) ... ))
     (let ([out #'(keyword-apply all-knowns
                      '(symbolic-unit.keyword ... unit.keyword ... )
                      (list (sym symbolic-unit.unit) ... value ...)
                      '())])
       (pretty-print (syntax->datum out))
       out)]))

(let-quants (g m))
;(let-quants (g m [kg 1]))  ; FIXME cases like mg/kg where there are clearly distinct black boxes in question
;(let-quants (g m [mol 100]))  ; FIXME units without 1char abbrevs
(let-quants ([L 100]))
;(let-quants (g m [L 100]))  ; FIXME kwarg ordering :/

(define (test-quants)
  ;(define final-volume 10)
  (let-quants ([g ?]  ; FIXME prefer this syntax due to the fact that it is eaiser to change what is known/unknown? support both?
               [L final-volume]
               ;[g/mol 58.4]  ; TODO
               [kg 10])))

(define (test-quants-will-fail)
  ; the fact that this errors when not qualified is great
  (let-quants ([g ?] [kg 10])))

'(define (test-quants-qualified)
   ; not working right due to the exactness of units problem
  (let-quants ([mg:ketamine-xylazine ?]
               [kg:mouse 0.04])))

(define (if-number-exact? maybe-number)
  (when (racket:number? maybe-number)
    (when (inexact? maybe-number)
      (error (format
              "~a is not exact! Please use exact numbers or rosette will cry ;_;"
              maybe-number)))))

(define (inexact?->exact maybe-number)
  (if (racket:number? maybe-number)
      (if (inexact? maybe-number)
          (let ([number (inexact->exact maybe-number)])
            ;(println (format "~a is not exact! Converted to ~a" maybe-number number))
            number)
          maybe-number)
      maybe-number))
  
(define (pass-knowns function kwargs)
  (define-values (keywords all-keywords) (procedure-keywords function))
  (pretty-print
   ; NOTE to self, the messing with list flattening is in practice-talk.org
   ; it is append* which is not a relevant here
   (flatten (for/list ([kw all-keywords]
                       [arg (map exact->inexact kwargs)]) `(,kw ,arg))))
  
  ;(pretty-print all-keywords)
  ;(pretty-print keywords)
  (keyword-apply all-knowns
                 (if (not (null? all-keywords))
                     all-keywords
                     keywords  ; need to use this as we trasition to #:g [g (sym g)]
                     )
                 kwargs
                 '()))

;(define (solve-molarity M g L g/mol)
(define (-solve-molarity #:M [M (sym M)]
                        #:g [g (sym g)]
                        #:L [L (sym L)]
                        #:g/mol [g/mol (sym g/mol)]
                        #:mol [mol (sym mol)])
  "Many unsat issues that can arrise in here come from exact/inexact issues.
This function should be wrapped with make-func-safe"

  ; TODO support mM input?
  ; TODO we should be able to use a computer algebra system to align the equations for us?
  (clear-asserts!)  ; FIXME may not want this if we generate a module?

  ;(define-symbolic mol real?)

  ;(define formals (list M g L g/mol))
  ;(define formals (list M g L g/mol mol))

  (define kwargs (list L M g g/mol mol))  ; FIXME auto sort by kwarg order??

  ;(map if-number-exact? formals)
  ;(pretty-print formals)

  ;(milli M)

  ; ddeeeerp y u no werk (because we set! the name to a make-keyword-procedure)
  (pass-knowns -solve-molarity kwargs)

  ;(all-knowns #:g g #:M M #:L L #:g/mol g/mol #:mol mol)

  #|
  (assert (>= g 0))
  (assert (>= L 0))
  (assert (>= M 0))
  (assert (>= mol 0))
  (assert (>= g/mol 0))

  (assert (= M (/ mol L)))
  (assert (= g/mol (/ g mol)))

  (assert (= M (/ (/ g L) g/mol)))
  |#
  (return-solved kwargs))

(define (return-solved kwargs)
  (define hashout (model (solve null)))
  ;(pretty-print (asserts))
  ;(for ([(k v) (in-dict hashout)] #:when (memv k kwargs)) (pretty-print k))

  ;(pretty-print hashout)
  ;(pretty-print (for/hash ([(k v) (in-dict hashout)]) (values k (exact->inexact v))))

  (for/hash ([(k v) (in-dict hashout)] #:when (memv k kwargs)) (values k (exact->inexact v))))

;(trace -solve-molarity)
;(trace pass-knowns)
;(trace all-knowns)

(define-symbolic g real?)  ; defined above
(define-symbolic M real?)
(define-symbolic L real?)
(define-symbolic g/mol real?)
(displayln "1-----------------------------")

;(define -g/mol (inexact->exact 58.4))
;(define -g/mol (/ 5800000 99315))  ; a pretty good approximation 58.40004 my hand
;(define (make-safe number)
  ;(inexact->exact (real->single-flonum number))
  ;(inexact->exact number))

(define (make-func-safe func)
  (define-values (keywords all-keywords) (procedure-keywords func))
  ;(pretty-print func-keywords)
  (define new-func (make-keyword-procedure
                    (λ (kw kwargs . rest)
                      ; clear asserts here
                      ;(clear-asserts!)
                      ;; inject all-knows
                      ;()
                      (let-values ([(keyword-list arg-list) (for/lists (l0 l1)
                                               ([key kw]
                                                [arg kwargs]
                                                #:when (member key all-keywords))
                                               (values key (inexact->exact arg)))])
                        (pretty-write `(keyword-list ,keyword-list))
                        (pretty-write `(arg-list ,arg-list))
                        (keyword-apply func
                                       keyword-list
                                       arg-list
                                       '())))))
  new-func)

(define solve-molarity (make-func-safe -solve-molarity))
;(trace -solve-molarity)

(solve-molarity #:M 0.01 #:g g #:L 2 #:g/mol 58.4 #:qq 'lol)
(solve-molarity #:M M #:g 10 #:L L #:g/mol 58.4)
(solve-molarity #:M M #:g 10 #:L 1 #:g/mol 58.4)
(solve-molarity #:M M #:g 10 #:L 1.0 #:g/mol 58.4)
(solve-molarity #:M M #:g 10 #:L 2 #:g/mol 58.4)

(define (-solve-N #:N [N (sym N)] #:kg [kg (sym kg)]
                  ; YES WE CAN! :D convert this into something auto generated! :)
                  #:m [m (sym m)] #:s [s (sym s)] #:g [g (sym g)])
  (clear-asserts!)
  ;(define-symbolic g real?)
  ;(assert (= (kilo g) kg))
  (define kwargs (list N g kg m s))
  (pass-knowns -solve-N kwargs)
  (return-solved kwargs))
(define solve-N (make-func-safe -solve-N))

(procedure-keywords -solve-N)

(displayln "2----------------------------------")

;(sym kg)
(solve-N #:N (sym N) #:kg 10 #:m 300 #:s 3)
(solve-N #:kg 100 #:m 300 #:s 3)

(module+ new
  ;; given a single equation written as an s-expression
  ;; automatically generate the define-symbolics and the asserts
  ;; the symbolic defines need to be qualified and matched to the individual
  ;; real things to which they are bound otherwise they are assumed to be
  ;; the same quantity

  ;; the other issue is that we might have to do multiple dispatch ...
  '((solution ml)
    (subject kg)
    (performance h)
    (/ (/ ml kg) h)  ; need to check for cases where there are multiple instances of the same unit
    (/ (/ volume mass) duration)  ; would be nice to specify these equations by dimension and specialize to units ...
    (ml/k/h)
    )

  (define-symbolic volume real?)
  (define-symbolic mass real?)
  (define-symbolic duration real?)
  (define-symbolic invariant real?)  ; simpler than constructing the equation-name every time
  #;
  (define-symbolic volume/mass/duration real?)
  ; I don't think we can lift to dimensions here ... maybe rosette can IF we specify the base system (e.g. mgs)
  ; wrong, we can, because the dimensional relationship is what matters in here, the units
  ; are tracked by the caller and this function doesn't have to know anything about them
  #;
  (define-symbolic ml/kg/h real?)
  (clear-asserts!)
  ; 30 ml/kg/hour -> how many ml in 3 hours for 100kg? (* 30 3 100) = 9000ml = 9L
  #;
  (assert (= 30 ml/kg/h))  ; FIXME vs volume/mass/duration n I don't think we can

  ; dimensional
  ;(assert (< 0 volume))
  ;(assert (< 0 mass))
  ;(assert (< 0 duration))
  (assert (and (< 0 volume) (< 0 mass) (< 0 duration) (< 0 invariant)))
  (assert (= invariant (/ (/ volume mass) duration)))
  #;
  (assert (= volume/mass/duration (/ (/ volume mass) duration)))


  (define-symbolic solution-ml real?)
  (define-symbolic subject-kg real?)
  (define-symbolic performance-h real?)  ; it will always be step/performance right? so could be implicit? well no there is hours / day

  #;
  (assert (= 30 volume/mass/duration))

  (assert (= invariant 30))
  (assert (= volume solution-ml))
  (assert (= mass subject-kg))
  (assert (= duration performance-h))
  ;(assert (= ml/kg/h (/ (/ solution:ml subject:kg) performance:h)))
  ;(assert (= ml/kg/h (/ (/ volume mass) duration)))
  (model (solve (assert (and (= subject-kg 100) (= performance-h 3)))))
  )

(module helper racket/base
  (require (only-in racket/list flatten))
  (provide get-variables)
  (define (get-variables equation)
   (flatten (get-cdrs equation)))

  (define (get-cdrs l)
   ; recursively return all the cdrs
   (cond [(null? l) l]
         [(syntax? l) (let ([decomp (syntax-e l)])
                        (if (list? decomp) (get-cdrs decomp) l))]
         [(list? l) (map get-cdrs (cdr l))]
         [#t l]))

  (module+ test
    (require rackunit)
    (check-equal? (get-cdrs '(1 2 (3 (4 5 6) (7 8 9)))) '(2 ((5 6) (8 9))))
    (define equation-1 '(/ (/ volume mass) duration))
    (define equation-2 '(/ (/ volume (* mass (expt acceleration lol))) duration))
    (get-variables equation-1)
    (get-variables equation-2)
    (define eqn #'(/ (/ volume (* mass (expt acceleration lol))) duration))
    (get-variables eqn)
    (define eqn-2 #'((= molar-mass (/ mass-atomic mol-atomic))
                     (= mol-atomic 1)))
    (get-variables eqn-2)
    ))

(require (for-syntax 'helper racket/pretty))

(begin-for-syntax
  (require (for-syntax syntax/parse))
  (define-syntax (pp stx)
    (syntax-parse stx
      [(_ body:expr ...)
       #'(let ([out (begin body ...)])
           (if (syntax? out)
               (pretty-print (syntax->datum out))
               (pretty-print out))
           out)])))

(define-syntax (invariant-ok stx)
  (syntax-parse stx
    [(_ equation)
     #:with (variable ...) (get-variables #'equation)  ; so apparently using syntax->datum on #'equation destorys everything ...
     #:with (variable-value ...) (map gensym (syntax->datum #'(variable ...)))
     #:with (variable-keyword ...) (map (compose string->keyword symbol->string)
                                        (syntax->datum #'(variable ...)))
     #'(λ (#:invariant invariant-value (~@ variable-keyword [variable-value (sym variable)]) ...)
         (clear-asserts!)
         (define-symbolic variable real?) ...
         (define-symbolic equation-invariant real?)
         (assert (and (< 0 variable) ...))
         (assert (= equation-invariant equation))
         (assert (and (= equation-invariant invariant-value) (= variable variable-value) ...))
         (model (solve null))
         )]))

(define-syntax (invariant-good stx)
  (syntax-parse stx
    [(_ equation)
     #:with (variable ...) (get-variables #'equation)
     #:with (variable-value ...) (map gensym (syntax->datum #'(variable ...)))
     #:with (variable-keyword ...) (map (compose string->keyword symbol->string)
                                        (syntax->datum #'(variable ...))) ;; TODO check if the syntax loc is tracked
     #;(map (λ (v) (format-id v)) (syntax->datum #'(variable ...)))
     #'(λ (#:invariant invariant-value (~@ variable-keyword [variable-value (sym variable)]) ...)
         (clear-asserts!)
         (define-symbolic variable real?) ...
         (define-symbolic equation-invariant real?)
         (assert (and (< 0 variable) ...))
         (assert (= equation-invariant equation))
         (assert (and (= equation-invariant invariant-value) (= variable variable-value) ...))
         (model (solve null))
         )]))

(define-syntax (invariant stx)
  (syntax-parse stx
    [(_ equation helper-assertion ...)
     #:do
     ((define variables (get-variables #'equation))
      (define var-syms (map syntax-e variables))
      (define helpers (filter (λ (h) (let ([sh (syntax-e h)])
                                       (and #t (symbol? sh)
                                            (not (member sh var-syms)))))
                              (remove-duplicates
                               (get-variables #'(no-op helper-assertion ...))
                               (λ (a b) (equal? (syntax-e a) (syntax-e b)))
                               )))
      (pretty-print `(helpers: ,helpers))
      (define var-hel (filter (compose symbol? syntax-e) (append variables helpers))))
     #:with (variable ...) var-hel ;variables
     ;#:with (variable ...) (append (get-variables #'equation) (get-))
     ;#:with (hvariable ...) (get-variables #'(helper-assertion ...))
     ;#:with (variable-value ...) (map gensym var-hel)
     #:with (variable-value ...) (map gensym (syntax->datum #'(variable ...)))
     #:with (variable-keyword ...) (map (compose string->keyword symbol->string)
                                        (syntax->datum #'(variable ...))) ;; TODO check if the syntax loc is tracked
     ; if a helper uses the same id as the main equation it will be a different syntax object so we have to strip the source
     ;#:with (helper-matched ...) (syntax->datum #'(helper-assertion ...))
     #;(map (λ (v) (format-id v)) (syntax->datum #'(variable ...)))
     (pp
     #'(λ (#:invariant invariant-value (~@ variable-keyword [variable-value (sym variable)]) ...)
         (clear-asserts!)
         (define-symbolic variable real?) ...
         (define-symbolic equation-invariant real?)
         (assert (and (< 0 variable) ...))
         (assert helper-assertion) ... ; include helper equations that have additional useful relations
         (assert (= equation-invariant equation))
         (assert (and (= equation-invariant invariant-value) (= variable variable-value) ...))
         #;
         (model (solve null))
         (begin0
             (model (solve null))
           (when #t  ; TODO are there use cases where we don't want to clear asserts?
             (clear-asserts!))))
     )
     ]
    #; ; TODO
    [(_ (operator:id equation-invariant:id equation:expr))
     ; allowing the user to specify the invariant id and the operator will simplify composition
     ; there are then 3 ways we could pass values, args, kwargs, and parameters
     ; that is going to depend on what we ultimately need/want to do with these
     ; I think the procedures need to be stored attached to their steps/participants/inputs/outputs or similar
     ; so that we can implement automatic conversion, NOTE that the form we have now does allow for
     ; more effective composition and reuse and the effect has clear racket semantics
     ; so I suggest that we retain this form, and we can then use a another macro to
     ; implement the (invariant (<= aspect (/ a b)))
     ; it would be really great if it were possible to compose the output functions
     ; such that you could do (< molarity 10) instead of just (= molarity 10) but
     ; I'm not sure that actually helps us since we basically always have to compute the upper bounds
     #'(λ (equation-invariant)
         (assert (operator equation-invariant equation)))]
    ))

(module+ test
  (define dose-rate (invariant (/ (/ volume mass) duration)))
  (dose-rate #:invariant 30 #:mass 100 #:duration 3)
  ; the keyworded versions is a slightly more programatic api
  ; but we should also be able to accept/process a form that
  ; fills in the blanks ; i.e., we can use hole syntax
  '
  (dose-rate (= 30 (/ (/ volume 100) 3)))

  ; NOTE to implement this I think we actually need to allow multiple invariants
  ; in a single space so that (= molar-mass (/ mass mol)) can be provided as well
  (define molarity (invariant #;(/ (* mass (/ mass-molar)#;(/ mol mass-atomic)) volume)
                              (/ (* mass (/ mass-molar)) volume)
                              (= mass-molar (/ mass-atomic mol-atomic))
                              ;(= mol 1)
                              (= mol-atomic 1)
                              ))
  (molarity #:invariant 100 #:mass-molar 100 #:volume 2) ; life pro-tip: don't try to make a 100M solution out of something that weighs 2g/mol :D 20 kiols of the stuff ... what is it uranium!?
  #; ; yeah sadly can't inline this without doing the rewrite
  (define M (invariant (/ (* mass (/ (= mass-molar
                                        (/ mass-atomic
                                           mol-atomic)))
                             volume))))
  #;
  (M #:invariant 100 #:mass-molar 100 #:volume 2)
  )

(module+ new-simple
  (clear-asserts!)

  ; a general dose per hour relation
  (define-symbolic volume real?)
  (define-symbolic mass real?)
  (define-symbolic duration real?)
  (define-symbolic invariant real?)
  (assert (and (< 0 volume) (< 0 mass) (< 0 duration) (< 0 invariant)))
  (assert (= invariant (/ (/ volume mass) duration)))

  ; the specialization of that can actually drop the units entirely
  ; for interaction with this function, conversion to other units
  ; can be handled elsewhere in a context that cares about the units
  ; we don't care about those here, we only care about quickly solving
  ; to return the necessary amounts of different inputs
  (model (solve (assert (and (= invariant 30) (= mass 100) (= duration 3)))))

  )
