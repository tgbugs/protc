#lang racket/base

(require
 "direct-model.rkt"
 rdf/utils
 (for-syntax
  racket/base
  racket/path
  racket/syntax
  syntax/parse
  "utils.rkt"
  "identifier-functions.rkt"
  (except-in "direct-model.rkt" #%top)
  "syntax-classes.rkt"))

(provide (all-defined-out)
         (rename-out [input implied-input]  ; this does not prevent the export of input as input
                     #; ; not entirely clear how we want to deal with #%top
                     [rdf-top #%top])
         (all-from-out "direct-model.rkt"))

(module+ test
  (require rackunit
          "utils.rkt"
          "syntax-classes.rkt"
          syntax/parse
          racket/function
          syntax/macro-testing
          (for-syntax racket/function)))

; aspect and input are the only 2 that need to lift units out

(define-syntax (executor-verb stx)
  (syntax-parse stx
    [act:sc-cur-executor-verb
     ;; TODO check whether the verb is know or whether we need to
     ;; add it to a list of unspecified terms
     #'act]))

(define-syntax (actualize stx)
  (syntax-parse stx
    [thing #;(_ wut:id (~optional (~seq #:prov prov)) rest:expr ...)
           #''thing]))

(define-syntax (input stx)
  (syntax-parse stx
    [sec:sc-cur-input
     ; OK is there a way to reference down two dots?
     ; no, you have to do the renaming inside the syntax class
     ; which is a good thing, it provides an enforced abstraction boundary
     #:with black-box (if (attribute sec.name)
                          (datum->syntax #'sec.name (string->symbol (syntax-e #'sec.name)))
                          ; FIXME type vs token ...
                          ; FIXME spaces!?
                          (datum->syntax #'sec.term (string->symbol (syntax-e #'sec.term-label)))
                          )
     #'(actualize black-box #:prov (hyp: 'sec.prov-id)
                  (~? sec.inv-lifted) ...
                  (~? sec.par-lifted) ...
                  (~? sec.asp (raise-syntax-error "HOW?!")) ...
                  ;(~? body (raise-syntax-error "HOW?!")) ...
                  )
     ]))
(module+ test
  ; default interpretation of nested inputs is a make spec that only lists inputs
  (input "top level thing" (hyp: 'p)
         (input "input to tlt 1" (hyp: '1))
         (input "input to tlt 2" (hyp: '2))))

(define-syntax (output stx)
  (syntax-parse stx
    #:datum-literals (hyp: quote spec make)
    #:literal-sets (protc-fields protc-ops)  ; whis this not working?
    [(_ (~or* name:str term:sc-cur-term) (hyp: (quote id))
        (~alt unconv:str
              asp:sc-cur-aspect
              inv:sc-cur-invariant
              par:sc-cur-parameter*
              bbc:sc-cur-bbc
              input:expr) ...)
     #:with spec-name (if (number? (syntax-e #'id))
                          (fmtid "_~a" #'id)  ; recall that #'_id doesn't work because the type is not strictly known
                          #'id)
     #:with black-box (if (attribute name)
                          (datum->syntax #'name (string->symbol (syntax-e #'name)))
                          ; FIXME type vs token ...
                          ; FIXME spaces!?
                          (datum->syntax #'term (string->symbol (syntax-e #'term.label)))
                          )
     #'(define-make (spec-name black-box)
         "this is a docstring from curation!"
         #:prov (hyp: 'id)
         ;#:vars (what is the issue here folks)
         ; othis stuff works at top level ...
         ;#:inputs (input ...)
         ;#:constraints ((~? asp) ... (~? par) ...)
         #:inputs (input ...)
         #:constraints ((~? asp) ... (~? par.lifted) ... (~? inv.lifted) ...)
         )
     ]))
(module+ test
  (output "thing" (hyp: 'prov-a)
          (parameter* (quantity 100 (unit 'meters 'milli))
                      (hyp: 'prov-b)))
  )

(define-syntax (black-box-component stx)
  (syntax-parse stx
    [section:sc-cur-bbc
    #'(quote section)  ; TODO FIXME
    ]))

(define-syntax (vary stx)
  ; when used inside an aspect this will be lifted and loop inverted
  ; since it indicates that FOR THE SAME SUBJECT all of these values
  ; are to be explored
  (syntax-parse stx
    #:datum-literals (hyp: quote)
    [(_ (~or* name:str term:sc-cur-term) (hyp: (quote id))
        (~alt unconv:str
              par:sc-cur-parameter*
              inv:sc-cur-invariant) ...)
     #`(quote #,stx)]))
(module+ test
  (vary "variable-name" (hyp: '-1)
        (parameter* (quantity 10) (hyp: '-10)))
  (vary "variable-name" (hyp: '-2)
        (invariant (quantity 20) (hyp: '-11)))
  (vary "variable-name" (hyp: '-3)
        (parameter* (quantity 10) (hyp: '-12))
        (invariant (quantity 20) (hyp: '-13))))

(define-syntax (aspect stx)
  (syntax-parse stx
    #:datum-literals (hyp: quote)
    [section:sc-cur-aspect
     #:with (errors ...)
     (make-errors [(or (attribute section.asp)
                       (attribute section.inv)
                       (attribute section.par)
                       (attribute section.mes)
                       (attribute section.cal)
                       (attribute section.res)
                       (attribute section.var))
                   stx
                   (format "WARNING: Aspect missing body in ~a at ~a ~a ~a"
                           (syntax->datum #'section)
                           (let ([src (syntax-source #'(~? section.name section.term))])
                             (if (not (or (path-string? src) (path-for-some-system? src)))
                                 'stdin
                                 (file-name-from-path src)))
                           (syntax-line #'(~? section.name section.term))
                           (syntax-column #'(~? section.name section.term)))
                   #:kind protc-missing-section
                   #:fix #t  ; TODO
                   ]
                  [(not (attribute section.warning))
                   stx
                   (format "WARNING: ~a at ~a ~a ~a"
                           (syntax->datum #'(~? section.warning "There is no warning."))
                           ; FIXME section.term.label ...
                           (let ([src (syntax-source #'(~? section.name section.term))])
                             (if (not (or (path-string? src) (path-for-some-system? src)))
                                 'stdin
                                 (file-name-from-path src)))
                           (syntax-line #'(~? section.name section.term))
                           (syntax-column #'(~? section.name section.term))
                           )
                   #:kind protc-missing-section
                   ])
     ; TODO check that the given unit matches
     #`(begin errors ... (quote #,stx))]
    [section:sc-cur-aspect-bad
     #:with (errors ...) (make-errors [#f
                                       stx
                                       (format "WARNING: Aspect has zero or multiple entries in\n~a at ~a ~a ~a"
                                               #'section
                                               ; FIXME section.term.label ...
                                               (let ([src (syntax-source #'(~? section.name section.term))])
                                                 (if (not (or (path-string? src) (path-for-some-system? src)))
                                                     'stdin
                                                     (file-name-from-path src)))
                                               (syntax-line #'(~? section.name section.term))
                                               (syntax-column #'(~? section.name section.term)))
                                       #:kind protc-warning  ; TODO
                                       #:fix #t])
     #'(begin errors ...)]))
(module+ test
  (aspect "mass" (hyp: '0))
  (aspect "test-unconv" (hyp: 'lol) "unconverted")
  (aspect "test-measure" (hyp: 'lol)
          (*measure "measure something!" (hyp: 'hrm)))
  (aspect "bob"
          (hyp: '1)
          (parameter*
           (quantity
            10
            (unit 'kelvin 'milli))
           (hyp: '2)))
  (aspect "holding potential"
          (hyp: '3)
          (vary "holding-potential" (hyp: '3.5)
                (parameter*
                 (quantity -70 (unit 'volts 'milli))
                 (hyp: '4))
                (parameter*
                 (quantity -50 (unit 'volts 'milli))
                 (hyp: '5))))
  (aspect "angle"
          (hyp: 'yes)
          (black-box-component "start from here thing" (hyp: 'asdf))  ; TODO auto lift?
          (parameter* (expr (range (quantity 1) (quantity 2 (unit 'meters 'mega)))) (hyp: 'fdsa)))
  (aspect "sagittal" (hyp: 'asdf)
          ; OK
          ; this is ok because the spatial-1d aspect is the direct connection to the parameter*
          ; and thus the plane of section is assumed to give unambiguously the axis normal to it
          ; thus to interpret the values we have to walk from inside out and resolve the
          ; ambiguity from the number out to the 3d black box (oof assumptions)

          ; saggital and thick probably need to be re-ordered? FALSE protc/ur is backwards
          ; because thick -> spatial-1d applied to a 3d black box needs at least a plane of section
          ; technically these do not commute, but composition is the wrong operation
          ; (compose spatial-1d spatial-2d) vs (compose spatial-2d spatial-1d)
          ; in the first case a length of a thing normal to the plane of section
          ;  since anything in-plane would be ambiguous
          ; in the second case either we have to invert this
          ;  or we have to assume that we are missing at least one spatial-1d axis aspect
          ;  and we might be missing more
          ; the situation is more complex if the spatial-2d is an extent rather than an infinite plane
          ; if we are specifying a spatial-2d then minimally we need two aspects, one that is spatial-1d
          ; e.g. sagittal is (plane-normal-to medial-lateral-axis) or (plane-coplanar-with a-p d-v)
          ; if our black box is known to be a plane (which it might not) then we only need (a-p d-v)
          (aspect "thick" (hyp: 'fdsa)
                  (parameter*
                   (quantity 8 (unit 'meters 'micro)) (hyp: 'a))))
  (aspect "will fail multibody" (hyp: 'asdf)
          (parameter* (quantity 1) (hyp: '1))
          (parameter* (quantity 2) (hyp: '2)))
  (aspect (term ilxtr:lol "lol" #:original "laugh out loud") (hyp: '0))
  #;
  (aspect "some aspect" (hyp: 'asdf)
          (no-how-error)))

(define-syntax (unit stx)
  (syntax-parse stx
    [(_ unit-base:sc-unit-name (~optional unit-prefix:sc-prefix-name))
     #`(quote #,stx)  ; TODO
     ]))

(define-syntax (unit-expr stx)
  (syntax-parse stx
    [(_ unit-expr:sc-unit-expr)
     #`(quote #,stx)  ; TODO
     ]))

(define-syntax (parameter* stx)
  (syntax-parse stx
    [_:sc-cur-parameter*
     #`(quote #,stx)]))

(define-syntax (invariant stx)
  (syntax-parse stx
    [_:sc-cur-invariant
     #`(quote #,stx)]))

(define-syntax (result stx)
  (syntax-parse stx
    [_:sc-cur-result
     #`(quote #,stx)]))

(module+ test
  (parameter* (quantity 100 (unit 'meters 'milli)) (hyp: 'prov-0))
  (output "thing" (hyp: 'prov-1)
          (parameter* (quantity 100 (unit 'meters 'milli)) (hyp: 'prov-2)))
  (check-exn exn:fail:syntax? (thunk
                               (convert-syntax-error
                                (parameter* 100 (unit 'meters 'milli) (hyp: 'prov-1))))))

(define (objective* text prov) #f)
(define (telos text prov) #f)

(define (order) #f)
(define (repeate) #f)

(define (references-for-use) #f)
(define (references-for-evidence) #f)

(define (black-box) #f)

;;; TODO

(define-syntax (*measure stx)
  (syntax-parse stx
    [_:sc-cur-*measure
     #`(quote #,stx)]))

(define-syntax (calculate stx)
  (syntax-parse stx
    [_:sc-cur-calculate
     #`(quote #,stx)]))

(define-syntax (version stx)
  #''TODO)

(define-syntax (i-have-no-idea stx)
  #''TODO)

(define-syntax (para:dilution stx)
  #''TODO)

(module+ test
  (define-syntax (test stx)
    (syntax-parse stx
      [(_ thing:sc-cur-invariant)
       #''thing.lifted]))

  (check-exn exn:fail:syntax? (thunk
                               (convert-syntax-error
                                (test (parameter* (hyp: '0) (quantity 10 (unit 'meters)))))))

  (test (invariant (quantity 10 (unit meters)) (hyp: '0)))

  (input "thing" (hyp: '1) (parameter* (quantity 10 (unit meters)) (hyp: '2)))

  (define-syntax (unit-> stx)
    (syntax-parse stx
      [(_ name:expr prefix:expr)
       #:with n (fmtid "~a" (datum->syntax #'name (syntax-local-eval #'name)))
       #:with p (fmtid "~a" (datum->syntax #'prefix (syntax-local-eval #'prefix)))
       #'(unit n p)]
      ))

  (define (runtime-unit name prefix)
    "Check whether a unit is valid at runtime using the unit syntax-class"
    (with-syntax ([n name]
                  [p prefix])
      (syntax-parse #'(unit n p)
        [thing:sc-unit
         (syntax->datum #'thing)])))

  (define-values (indirect-meters indirect-milli) (values 'meters 'milli))

  (unit meters milli)
  (unit 'meters 'milli)  ; not entirely sure if we want this ... but ok
  ;(unit indirect-meters indirect-milli)  ; fails ...
  (unit-> 'meters 'milli)
  ;(unit-> indirect-meters indirect-milli)  ; fails ...

  (runtime-unit 'meters 'milli)
  (runtime-unit ((thunk 'meters)) ((thunk 'milli)))
  (check-exn exn:fail:syntax? (thunk (runtime-unit 'nota 'unit)))
  (runtime-unit indirect-meters indirect-milli)  ; this works
  (let ([m 'meters]
        [_m 'milli])
    (runtime-unit m _m)))
