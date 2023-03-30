#lang racket/base

(require
 "direct-model.rkt"
 rdf/utils
 "curation-unprefixed.rkt"
 (for-syntax
  racket/base
  racket/path
  racket/syntax
  syntax/parse
  "utils.rkt"
  #;
  "identifier-functions.rkt"
  (except-in "direct-model.rkt" #%top)
  ;"curation-unprefixed.rkt"
  "syntax-classes.rkt"))

(provide (all-defined-out)
         (rename-out [input implied-input]  ; this does not prevent the export of input as input
                     [aspect implied-aspect]
                     [output implied-output]
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
           (for-syntax racket/function))
  (define-syntax (check-fail-syntax stx)
    (syntax-parse stx
      [(_ body)
       #'(check-exn exn:fail:syntax? (thunk (convert-syntax-error body)))
       ])))

; aspect and input are the only 2 that need to lift units out

(define-syntax (make-placeholder-syntax stx)
  (syntax-parse stx
    [(_ stx-name)
     #:with elip (datum->syntax this-syntax '...)
     #'(define-syntax (stx-name stx)
         (syntax-parse stx ; TODO
           [(_ name (~optional (~seq #:prov prov)) body elip)
            #'(begin
                (quote (stx-name name (~? (~@ #:prov prov))))
                body elip)]))]))

; TODO for all of these
(make-placeholder-syntax black-box)
(make-placeholder-syntax input-instance)
(make-placeholder-syntax output-instance)
(make-placeholder-syntax symbolic-input)
(make-placeholder-syntax symbolic-output)
(make-placeholder-syntax executor) ; rare
(make-placeholder-syntax qualifier) ; rare
(make-placeholder-syntax objective*)
(make-placeholder-syntax telos)

(define (order) #f)
(define (repeate) #f)

(define (references-for-use) #f)
(define (references-for-evidence) #f)

#|
#;
(define-syntax (input-instance stx)
  (syntax-parse stx ; TODO
    [(_ name (~optional (~seq #:prov prov)) body ...)
     #'(begin (input-instance body ...))]))


(define-syntax (output-instance stx)
  (syntax-parse stx ; TODO
    [(_ body ...)
     #'(quote (input-instance body ...))]))

(define-syntax (symbolic-input stx)
  (syntax-parse stx ; TODO
    [(_ body ...)
     #'(quote (symbolic-input body ...))]))

(define-syntax (symbolic-output stx)
  (syntax-parse stx ; TODO
    [(_ body ...)
     #'(quote (symbolic-output body ...))]))

(define-syntax (executor stx) ; rarely used explicit mention of who should be doing what
  ; TODO
  (syntax-parse stx
    [(_ body ...)
     #'(quote (executor body ...))]))
|#

(define-syntax (transform-verb stx)
  (syntax-parse stx
    [(_ body ...)
     #'(quote (transform-verb body ...))]))

(define-syntax (executor-verb stx)
  (syntax-parse stx
    [act:sc-cur-executor-verb
     ; TODO check whether the verb is know or whether we need to
     ; add it to a list of unspecified terms
     #; ; XXX DO NOT DO THIS it will go infinite and there is no warning!
     #'act
     ; TODO verbs need macros that will expand them to their correct form
     ; we can do that in racket, but it escapes the dsl
     ; (ev "anes" (input "animal") (input "anesth")) ; the asymmetry is troublesome
     ; should be animal be the black box here? do we need primary-participant?
     ; XXX assume that verbs are normalized, error if a transformer is not defined or something ? order matters or type matters if we have it?
     ;#:with sigh (if #'act.prov (begin (println (list 'wat #'act.prov)) #'(#:prov act.prov)) #f)
     #'(begin ; FIXME TODO in the absense of a working implementation of transform-verb at least make sure that we expand and evaluate nested forms
         (transform-verb act.name (~? (~@ #:prov act.prov)))
         act.body ...
         )]))

(define-syntax (actualize stx)
  (syntax-parse stx
    [(_ black-box:id
        (~optional (~seq #:prov prov:sc-cur-hyp))
        body ...)
     #'(begin ; XXX for now expand the rest of the forms so we get error messages
         'black-box ; XXX many name errors for internal black boxes need to add binding context
         body ...)]
    #;
    [thing #;(_ wut:id (~optional (~seq #:prov prov)) rest:expr ...)
           #''thing]))

(define-syntax (input stx)
  (syntax-parse stx
    #:disable-colon-notation
    #:conventions (conv-ur-parts)
    #:literals (TODO aspect black-box-component circular-link input invariant objective* output parameter* param:parse-failure)
    [(_ (~or* name term)
        (~optional (~seq #:prov prov))
        (~alt unconv
              (~and (aspect _ ...) asp)
              (~and (invariant _ ...) inv) ; things that we are going to transform at this level can't just be check by their own macros probably
              (~and (parameter* _ ...) par)
              (~and
               ((~or* TODO aspect black-box-component circular-link input invariant objective* output parameter* param:parser-failure)
                _ ...)
               nexpr)) ...)
     #:with black-box (if (attribute name)
                          (datum->syntax #'name (string->symbol (syntax-e #'name)))
                          ; FIXME type vs token ...
                          ; FIXME spaces!?
                          (datum->syntax #'term (string->symbol (syntax-e #'term.label)))
                          )
     #'(actualize black-box (~? (~@ #:prov prov))
                  (~? inv.lifted) ...
                  (~? par.lifted) ...
                  (~? asp (raise-syntax-error "HOW?!")) ...
                  (~? nexpr (raise-syntax-error "HOW?!")) ...
                  )

     ]
    #;
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
     #'(actualize black-box (~? (~@ #:prov sec.prov))
                  (~? sec.inv-lifted) ...
                  (~? sec.par-lifted) ...
                  (~? sec.asp (raise-syntax-error "HOW?!")) ...
                  ;(~? body (raise-syntax-error "HOW?!")) ...
                  )
     ]))

(module+ test
  ; default interpretation of nested inputs is a make spec that only lists inputs
  (input "top level thing" #:prov (hyp: 'p)
         (input "input to tlt 1" #:prov (hyp: '1))
         (input "input to tlt 2" #:prov (hyp: '2)))

  (check-exn
   exn:fail:syntax?
   (thunk
    (convert-syntax-error
     (parameter* "p1" 'wat))))

  (check-exn
   exn:fail:syntax?
   (thunk
    (convert-syntax-error
     (input "i1"
            (aspect "a1"
                    (parameter* "p1"
                                'oops-i-have-a-body
                                )
                    )

            ))))
  )

(define-syntax (output stx)
  ; FIXME use the syntax class to avoid duplication probably
  (syntax-parse stx
    #:disable-colon-notation
    #:literal-sets (#;ls-output-nest protc-fields protc-ops)  ; whis this not working?
    ; FIXME so #:literals works but  #:literal-sets doesn't SIGH SIGH SIGH
    #:conventions (conv-ur-parts)
    #:literals (TODO aspect black-box-component circular-link input invariant objective* output parameter* param:parse-failure)
    [(_ (~or* name term) (~optional (~seq #:prov prov))
        (~alt unconv
              (~and (input _ ...) in)
              (~and (output _ ...) out)
              (~and (aspect _ ...) asp)
              (~and (parameter* _ ...) par)
              (~and (invariant _ ...) inv)
              ((~or* TODO aspect black-box-component circular-link input invariant objective* output parameter* param:parse-failure)
               ; if a syntax class does not match above we want to fall through here and still match so that we
               ; can get a clear error message as far down the hierarchy as possible
               nbody ...)
              ; FIXME allowing nested outputs is a problem because because those are defacto
              ; a new nested protocol burried inside a step of a single other protocol
              ; while in principle it would be nice to allow such nesting, it would mean that
              ; nested outputs would need scoping rules, which we are not prepared to deal with
              ; that kind of composition is also problematic because there are implicit order
              ; restrictions on processes that are nested in this way, finally lifting outputs
              ; out to the top level is not exactly straight forward but can be done where the
              ; output spec will be replaced by a reference to the output type as an input
              ; in short, for now we are going to leave those as syntax errors because they are
              ; nightmares to deal with and they break expansion of spec via define-make due to
              ; nesting begins inside the #:input keyword of define-make (it seems)
              ; XXX preserving the comments above with a note that I ended up implementing
              ; lifting them out to top level and adding the nested outputs to the inputs list
              ; and I do it at this stage becuase the right time to do this is when converting from
              ; protc/ur to direct-model or protc/base or whatever because this is the point at
              ; which we know that things should be flattened and how they were originally nested
              #;
              other:expr) ...)
     #:with spec-name (if (attribute prov.id)
                          (if (number? (syntax-e #'prov.id))
                           (fmtid "_~a" #'prov.id)  ; recall that #'_id doesn't work because the type is not strictly known
                           #'prov.id)
                          (fmtid "spec-~a" ; FIXME we need gensym here too
                                 (if (attribute name)
                                     #'name
                                     (datum->syntax #'term (string->symbol (syntax-e #'term.label)))))
                          )
     #:with black-box (if (attribute name)
                          (datum->syntax #'name (string->symbol (syntax-e #'name)))
                          ; FIXME type vs token ...
                          ; FIXME spaces!?
                          (datum->syntax #'term (string->symbol (syntax-e #'term.label))) ; FIXME term.label can be false
                          )
     #'(begin
         (define-make (spec-name black-box)
          "this is a docstring from curation!"
          (~? (~@ #:prov prov))
          ;#:vars (what is the issue here folks)
          ; othis stuff works at top level ...
          ;#:inputs (input ...)
          ;#:constraints ((~? asp) ... (~? par) ...)
          #:inputs (in ... (input out.name (~? (~@ #:prov out.prov))) ...)
          #:constraints ((~? asp) ... (~? par.lifted) ... (~? inv.lifted) ...)
          ;other ...
           )
         out ...
         )
     ]))

(module+ test
  (output "thing" #:prov (hyp: 'prov-a)
          (parameter* (quantity 100 (unit 'meters 'milli))
                      #:prov (hyp: 'prov-b)))
  (output "t1"
          (output "t2")
          )
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
    [(_ (~or* name:nestr term:sc-cur-term) (~optional (~seq #:prov prov:sc-cur-hyp))
        (~or*
         (~seq unconv:str ...)
         (~seq inv:sc-cur-invariant ...)
         (~seq par:sc-cur-parameter* ...)))
     #`(quote #,stx)]))

(module+ test
  (vary "variable-name" #:prov (hyp: '-1)
        (parameter* (quantity 10) #:prov (hyp: '-10)))
  (vary "variable-name" #:prov (hyp: '-2)
        (invariant (quantity 20) #:prov (hyp: '-11)))
  (check-exn
   exn:fail:syntax?
   (thunk
    (convert-syntax-error
     (vary "variable-name" #:prov (hyp: '-3)
           (parameter* (quantity 10) #:prov (hyp: '-12))
           (invariant (quantity 20) #:prov (hyp: '-13)))))))

(define-syntax (aspect stx)
  (syntax-parse stx
    #:disable-colon-notation
    #:local-conventions ([nexpr expr])
    #:conventions (conv-ur-parts)
    #:literals (TODO aspect black-box-component circular-link input invariant *measure output parameter* param:parse-failure vary)
    [(_ (~or* name term) (~optional (~seq #:prov prov))
        (~alt unconv
              ; note that the syntax class is vastly more complex right now because it tries to accomodate
              ; the case where aspect might also be a parent class, I'm 99% sure that we want a seprate
              ; tag for that, it still an aspect but possibly multi-aspect or something like that so that
              ; it is quick to check the tag, though obviously syntax-parse can bse used to support both
              ; patterns under a single tag, I'm pretty sure we don't want to do that
              (~and (aspect _ ...) asp) ; this case doesn't usually appear in the anno derived protcur set
              (~and (parameter* _ ...) par)
              (~and (invariant _ ...) inv)
              (~and (vary _ ...) var)
              (~and
               ((~or* TODO aspect circular-link invariant *measure parameter* param:parse-failure vary)
                ; if a syntax class does not match above we want to fall through here and still match so that we
                ; can get a clear error message as far down the hierarchy as possible
                _ ...)
               nexpr) ; bind the name so nested expansion can continue
              #;
              other:expr) ...)
     #:with recurse stx
     #'(begin
         (quote recurse)
         asp ...
         par ...
         inv ...
         var ...
         nexpr ...
         )]))

(define-syntax (aspect-alt stx)
  (syntax-parse stx
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
  (aspect "mass" #:prov (hyp: '0))
  (aspect "test-unconv" #:prov (hyp: 'lol) "unconverted")
  (aspect "test-measure" #:prov (hyp: 'lol)
          (*measure "measure something!" #:prov (hyp: 'hrm)))
  (aspect "bob"
          #:prov (hyp: '1)
          (parameter*
           (quantity
            10
            (unit 'kelvin 'milli))
           #:prov (hyp: '2)))
  (aspect-alt "holding potential"
          #:prov (hyp: '3)
          (vary "holding-potential" #:prov (hyp: '3.5)
                (parameter*
                 (quantity -70 (unit 'volts 'milli))
                 #:prov (hyp: '4))
                (parameter*
                 (quantity -50 (unit 'volts 'milli))
                 #:prov (hyp: '5))))
  (aspect-alt "angle"
          #:prov (hyp: 'yes)
          (black-box-component "start from here thing" #:prov (hyp: 'asdf))  ; TODO auto lift?
          (parameter* (expr (range (quantity 1) (quantity 2 (unit 'meters 'mega)))) #:prov (hyp: 'fdsa)))
  (aspect "sagittal" #:prov (hyp: 'asdf)
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
          (aspect "thick" #:prov (hyp: 'fdsa)
                  (parameter*
                   (quantity 8 (unit 'meters 'micro)) #:prov (hyp: 'a))))
  (aspect "will fail multibody" #:prov (hyp: 'asdf)
          (parameter* (quantity 1) #:prov (hyp: '1))
          (parameter* (quantity 2) #:prov (hyp: '2)))
  (aspect (term ilxtr:lol "lol" #:original "laugh out loud") #:prov (hyp: '0))
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

(module+ test
  (invariant (param:parse-failure less than half that of) (rest "less than half that of"))
  (check-fail-syntax (invariant (param:parse-failure less than half that of) "less than half that of")))

(define-syntax (result stx)
  (syntax-parse stx
    [_:sc-cur-result
     #`(quote #,stx)]))

(module+ test
  (parameter* (quantity 100 (unit 'meters 'milli)) #:prov (hyp: 'prov-0))
  (output "thing" #:prov (hyp: 'prov-1)
          (parameter* (quantity 100 (unit 'meters 'milli)) #:prov (hyp: 'prov-2)))
  (check-exn
   exn:fail:syntax?
   (thunk
    (convert-syntax-error
     (parameter* 100 (unit 'meters 'milli) #:prov (hyp: 'prov-1))))))

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
                               (convert-syntax-error ; FIXME false aliasing here ... where (hyp: '0) is incorrectly accepted
                                (test (parameter* (hyp: '0) (quantity 10 (unit 'meters)))))))

  (test (invariant (quantity 10 (unit meters)) #:prov (hyp: '0)))

  (input "thing" #:prov (hyp: '1) (parameter* (quantity 10 (unit meters)) #:prov (hyp: '2)))

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
