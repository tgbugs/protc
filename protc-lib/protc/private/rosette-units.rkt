#lang rosette
(require racket/pretty
         racket/trace
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
  (require racket/string syntax/parse (for-syntax racket/base syntax/parse))
  (provide (all-defined-out))

  (define (split-prefix-unit stx)
    ;(println stx)
    (define p-u (symbol->string (syntax->datum stx)))
    (define len (string-length p-u))

    (cond [(= len 1) stx]
          [(= len 2)
           ; FIXME TODO this fails for cases where the names are not a single char!
           #`((quote #,(datum->syntax stx (string->symbol (substring p-u 0 1))))
              #,(datum->syntax stx (string->symbol (substring p-u 1 2))))]
          [(= len 3) (cond [(string-prefix? p-u "da") (error)] ; TODO enable for other 2 part char prefixes
                           [#t (error)]  ; TODO
                           )]))

  (define-syntax-class maybe-prefix-unit
    (pattern prefix+unit:id
             #:attr prefix (let-values ([(p u) (split-prefix-unit #'prefix+unit)]) p)
             #:attr quoted-prefix (let-values ([(p u) (split-prefix-unit #'prefix+unit)]) #`(quote #,#'p))
             #:attr unit (let-values ([(p u) (split-prefix-unit #'prefix+unit)]) u)
             #:attr keyword (let-values ([(p u) (split-prefix-unit #'prefix+unit)])
                              ; WARNING! that #f right there may try to destroy the universe again
                              (datum->syntax #f (string->keyword (symbol->string (syntax->datum u))))))))

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
  (syntax-parse stx
    [(_ prefix+unit:id)
     (with-syntax ([(prefix unit) (split-prefix-unit #'prefix+unit)])
       (println #'prefix)
       (println #'unit)
       (let ([out #'(assert (= prefix+unit (* unit (get-multiplier prefix))))])
         (pretty-print (syntax->datum out))
         out)
       )]))

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


(define ? #'DEADBEEF)

(define-syntax (let-quants stx)
  (syntax-parse stx #:literals (?)
    [(_ ([unit:maybe-prefix-unit (~or* ? value)] ... ))
     ; do the unit conversion outside the solver
     #'(all-knowns (~seq unit.keyword (if (not (eq? #f value))
                                          (prefix-convert unit.prefix value)
                                          (begin (when (not (null? unit.prefix))
                                                   (prefix-assert unit.prefix unit.unit))
                                                 (sym unit.unit)))) ... )]))
(let-quants ([g ?] [m ?]))

'(define (test-quants)
  (let-quants ([g ?]
               [L final-volume]
               [g/mol 58.4]
               [kg 10])
              'stuff))

(define (all-knowns
         ; singluar
         ; all units not appearing in this film should be set to 1 by default to help the solver
         #:g [g 1] #:m [m 1] #:s [s 1]
         #:L [L 1] #:mol [mol 1] #:M [M 1]
         #:N [N 1]
         ; compound  TODO we need a way to compute these on the fly...
         #:g/mol [g/mol 1] #:mol/L [mol/L 1] #:g/L [g/L 1]
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
  
  (pretty-print all-keywords)
  (pretty-print keywords)
  ;(println "aaaaaaaaaaaaaaaaaaaaaaa")
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

                      ;(println "bbbbbbbbbbbbbbbbbbbbbb")
                      ;(pretty-print all-keywords)
                      ;(pretty-print keywords)
                      ;(pretty-print kwargs)
                      (take  ; FIXME 0 this doesn't work if the missing kwarg is out of order!
                       (if (not (null? all-keywords))
                           all-keywords
                           keywords)
                       (length kwargs))
                      (pretty-print
                       (flatten (for/list ([kw all-keywords]
                                           [arg (map exact->inexact kwargs)])
                                  `(,kw ,arg))))

                      (keyword-apply func
                                     -keywords 
                                     (map inexact?->exact kwargs)
                                     '()))))
  new-func)

(define solve-molarity (make-func-safe -solve-molarity))
;(trace -solve-molarity)

(solve-molarity #:M 0.01 #:g g #:L 2 #:g/mol 58.4)
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
