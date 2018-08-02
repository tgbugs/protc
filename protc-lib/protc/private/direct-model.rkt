#lang racket/base
(require (for-syntax debug/repl))
(require ;scribble/srcdoc
         ;(for-doc scribble/base scribble/manual)
         protc/export
         ;protc/utils  ; this is private ...
         rdf/utils
         protc/private/utils
         (for-syntax racket/base
                     racket/list
                     racket/string
                     racket/pretty
                     racket/syntax
                     syntax/parse
                     protc/private/utils))

(provide spec
         impl
         has-part
         part-of
         has-member
         define-relation
         define-aspect
         export
         invariant)

;;; the protc phase model
;;; 0. writing time
;;; 0.5 naming time (each successive phase up to impl returns a macro that the next phase uses to add more information)
;;; 1. spec time << this is reall part of 'writing time'
;;; 2. impl time << this is reall part of 'writing time' but this can act as a 'static' or frozen run time, ie multiple impls...
;;; 3. export time
;;; 4. execution time (run time)

;;; invocation of specs (or do can you only invoke/call an impl?)
;;; a spec spec may be called from
;;; 1. the top level
;;; 2. inside of a (result (spec-name)) or (result (impl-name))


;;; information constraint language ICL would be an alternate name for protc
;;; if I were in a stuffy corporate >_<

(module syntax-classes racket/base
  (require syntax/parse
           racket/syntax
           racket/dict
           (for-template (only-in racket/base #%top #%app quote))
           (for-syntax racket/base))
  (provide (all-defined-out))
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
  
  )

(require (for-syntax 'syntax-classes))



(define (runtime-read name:aspect)
  ; TODO in cases where there is an external file
  ; or the dataset is too large or is external
  ; we need to compute and identity on the file
  ; store its name (InterLex lingo) and have the
  ; user sign the identity
  ; the underlying implementation could be as simple
  ; as stuffing a file in git, or using git annex and
  ; having the user sign the commit hash and storing
  ; whatever globally resolvable id we need for the repo
  '(struct/remote-data name identity)
  '(struct/quantity value units))
(define (runtime-protocol name:aspect) "current-protocol TODO")
(define (runtime-measure-name name:aspect) '(get-current-impl name:aspect))
(define runtime-executor (make-parameter null))  ; looks like a function
(struct struct/result (protc:value source:protocol source:measure-name executor datetime)
  #:inspector (make-inspector))

(define-syntax (define/symbol->being stx)
  ;; "actualize some aspect of a thing"
  (syntax-parse stx
    [(_ name-of-symbol/being (~optional (~seq #:name name)) aspect body ...)
     ; if #:name is left out then the default is set or overwritten with a warning? or should it error
     ; if #:name is set then it is essentially an implementation
     #'TODO]
    ))

(define-syntax (define/being->symbol stx)  ; define/being:aspect->symbol
  ;; "measure some aspect of a thing"
  (syntax-parse stx
    #:local-conventions ([aspect sc-aspect]
                         [body sc-being->symbol-body]
                         )  ; TODO
    [(_ name-of-symbol/being (~optional (~seq #:name name)) aspect body ...)
     ; if #:name is left out then the default is set or overwritten with a warning? or should it error
     ; if #:name is set then it is essentially an implementation
     
     ; TODO aspects need to intelligently request only numbers or numbers + units
     ; TODO either the last expression in the body returns the values or we read the aspect and make a generic version
     #:with repl-message (datum->syntax (format "please input ~a" #'aspect))
     #:with validate-func (if (null? (syntax->datum #'(body.validate ...)))
                              #'(λ rest (apply (aspect-validate aspect) rest))  ; assume aspects are structus w/ validate...
                              ; FIXME there needs to be only one validate....
                              ; I think I'm doing this wrong
                              #'(body.validate ...))
     #:with read-func (if (null? (syntax->datum #'(body.read ...)))
                          #'read
                          ; FIXME there should probably be only 1 body? or what?
                          #'(body.read ...))
     #:with func-name (format-id #'name-of-symbol/being
                                 #:source #'name-of-symbol/being
                                 "~a:~a"
                                 (syntax-e #'name-of-symbol/being)
                                 (syntax-e #'aspect))
     #'(define (func-name)  ; TODO this needs name needs to be constructed
         body ...  ; one way to do this is to force the user to define these at run time
         ; and then arror if they fail to do so... probably a bad approach
         (println repl-message)
         ; TODO make this speccable
         ; also we need to pick sane default behavior if no implementation specifies what to do...
         (validate-func read-func))]))

(define-syntax (bind/symbol->being stx)
  (syntax-parse stx
    [(_ name ...)
     ;#:with output (datum->syntax this-syntax (cons 'values (syntax->datum #'(name ...))))  ; TODO warn on duplicates
     #:with output #'(values 'name ...)  ; TODO warn on duplicates
     #'output]))

(define (name-aspect->define/being->symbol-func name-aspect)
  "I have no idea what this is supposed to do")

(define-syntax (being->symbol stx)
  (syntax-parse stx
    [(_ [name aspect] ...)
     #'(map name-aspect->define/being->symbol-func '((name aspect) ...))]
    [(_ name ...)

     #:with (fake-result ...)
     (map (λ (name)
            (datum->syntax this-syntax (format-symbol "result/fake-~a" name)))
          (syntax->datum #'(name ...)))
     #'(values (being name) ...)  ; use this if we need a more explicit type
     #'(values 'fake-result ...)
     ]))

(define-syntax (define/definition-function stx)  ; XXX deprecated
  (syntax-parse stx
    [(_ being/symbol [aspect-or-measuring-func-name expected-value] ...)
     ; so this is too simplistic because there could and should ultimately be
     ; multiple different compatible definitions of the same thing
     ; note also that aspect-or-measuring-func-name needs to be defined for
     ; the being/symbol in question?
     ; this is essentially subsumed by the use of (spec thing body ...)  ; where the body is as described here
     #''TODO
     ]
    ))
(define (being/symbol->defining-funcs being/symbol)
  (hash-ref #hasheq((mouse . (list (λ (var1) (eq? var1 1))
                                   (λ (var2) (eq? var2 2))))
                    (brain-slice . (list (λ () #t)))
                    (symbol/body-output-1 . (list (λ (var) #t)))
                    (symbol/body-output-2 . (list (λ (var) #t)))
                    )
            being/symbol
            (λ vars (format "WARNING: no defining function for ~a" being/symbol))

            ))

(define-syntax (validate/being stx)
  (syntax-parse stx
      [(_ being/symbol ...)
       #'(map being/symbol->defining-funcs '(being/symbol ...))
       ; this gives us the set of all the individual measurement functions + their values
       ; or rather it should ... it means that being/symbol->defining-funcs should return
       ; both the measurement functions and the expected/asccepted values from their evaluation
       ])
  )

(define-syntax (define-aspect stx)
  ; FIXME this is still not as useful as I would like ...
  (syntax-parse stx
    [(_ shortname:id name:id
        (~optional (~seq #:parent parent))
        constructive-definition:expr  ; FIXME need a way to look inside of these
        )
     #'(begin
         (define name (list parent 'constructive-definition))
         (define shortname name)  ; TODO rosette integration
         )
     ]
    )
  )

(define-syntax (define-participant stx)
  ; black boxes or participants
  ; this is only for top level it allows participant aspect value bindings to occur
  ; outside the scope of a spec, in which case we will lift it to an input?
  ; or do we ban free variables?
  (syntax-parse stx
    #:datum-literals (:)
    [;(_ name:id aspec-value:expr ...)
     ;(_ name:id (: operator aspect (~or value (range start stop) (~ value))) ...)
     ; TODO operator e-v pairing e.g. ~ asp 10 or +/- asp 3 or in asp (range -1 3)
     ; or in asp (list 1 20 "hello" "goodbye")
     ; probably would want to check contracts on the operators at this stage?
     ; also operator needs to be (or/c (-> boolean?))
     ;(or/c (-> bookean?)) ; need to review how to use these...
     (_ name:id (: operator aspect expected-value) ...)
     #:with check-name (format-id #'name
                                  #:source #'name
                                  "check-~a"
                                  (syntax-e #'name))
     #'(begin
         (define (check-name thing)
           (let ([result (dict-ref thing aspect)])
             (operator result expected-value)  ; TODO managing failure?
             ) ...
           )
         )
     ])
  )

(define (make-store)
  (define int-triples '())

  (define (add-triple triples)
    (set! int-triples (cons triple int-triples)))

  (define (add-triples triples)
    (set! int-triples (append triples int-triples)))

  (values add-triple add-triples (λ () int-triples)))

(define-values (add-triple add-triples get-triples) (make-store))

#;(define (has-part parent . children)
  ; TODO in spec context has-part needs to also define all the part names in local scope ...
  (set! triples (append (for/list ([child children]) (triple has-part parent child)) triples))
  )

(define (part-tree tree)
  ; TODO local binding and pull out the has part relation
  tree)

#;(define-syntax (has-part stx)
                (syntax-parse stx
                  [(_ subject objects ...+)
                   #'(begin
                       subject  ; sanity to make sure it is bound, it should be
                       (set! triples (append (list (triple 'has-part subject 'objects) ...) triples))
                       ; TODO if-defined HRM hard to work with in this context ...
                       ; maybe easier to ignore or simply remove .config-vars
                       ; if there is a relationship in the body that defines these for us?
                       (define-values (objects ...) (bind/symbol->being objects ...))
                       )
                   ]
                  )
                )

(define-syntax (define-relation stx)
  (syntax-parse stx
    ;#:datum-literals (|.|) ; breaks the meaning of . since it is ...+ not ... FIXME
    [(_ (relation-name:id parent:id rest:id)  ; . is dangerous ... #;. does not comment
        docstring:string)
     #;#'(define (relation-name subject . objects)
         (set! triples (append (for/list ([object objects]) (triple relation-name subject object)) triples)))
     #:with elip (datum->syntax this-syntax '...)
     #:with elip+ (datum->syntax this-syntax '...+)
     #:with objects (datum->syntax this-syntax 'objects-internal)
     (let ([out 
            #'(define-syntax (relation-name stx)
                (syntax-parse stx
                  [(_ subject objects elip+)
                   #'(begin
                       (define test
                         ; sanity to make sure it is bound, it should be
                         subject)
                       ; TODO create/lookup urls if/when they are bound elsewhere otherwise
                       ; relationship-name, subject, and objects all need to be assigned ids

                       (add-triples (list (triple 'relation-name 'subject 'objects) elip))
                       #;(set! int-triples (append (list (triple 'relation-name 'subject 'objects) elip)
                                                 int-triples))

                       ; TODO if-defined HRM hard to work with in this context ...
                       ; maybe easier to ignore or simply remove .config-vars
                       ; if there is a relationship in the body that defines these for us?
                       (define-values (objects elip) (bind/symbol->being objects elip))
                       )
                   ]
                  )
                )])
       #;(pretty-print (syntax->datum out))
       out)
     ]
    #;[(relation-name:id parent:id child:id)
     #'(define (relation-name subject object)
         (set! triples (cons (triple relation-name subject object) triples)))
     ]))

(define-relation (has-part parent children)
  "parent has multiple parts children, such as a car having
   doors or a cell having a nucleus and a cytoplasm")

(define-relation (part-of child parent)
  "opposite of has-part")

(define-relation (has-member child parent)
  "explicitly name a member of a composite entity")

; FIXME figure out why we can't use this outside of this file ...
(define-relation (wired-to a bs)
  "some kind of wire connects a and any number of bs"
  )

(define a 'a)
(has-part a b c d e)

#;(define-syntax (define-process stx)  ; define-verb (spec (process (verb args ...))) is the alternate form
  ; define-process creates functions that operate only on
  ; symbol/being inputs ie (-> symbol-being? )
  (syntax-parse stx
    [(_ (process-name:id input:id ...))]
    )
  )

#;(define-syntax (spec stx)
  ; aka spec-process
  (syntax-parse stx
    ; this overloads meanings
    ; we also need a way to control the exact
    ; definitions that are pulled in if we allow
    ; defining free variables outside the scope of a spec
    [(_ (make output-name))
     (_ (def participant-name))  ; this is internal and composed of multiple measure rules
     ; specialization of (mass mouse) or (measure mouse mass) (*: mouse mass) would need to warn if no generic was known
     (_ (def aspect-name))  ; this is covered by define-aspect
     (_ (measure aspect-name))  ; I don't think we allow this form? require (measure thing aspect-name) at least
     (_ (measure participant-name aspect-name))
     ; we should be able to infer make vs measure, because the type of the return would be known?
     ; the question is whether spec is atomic, and I think that it has to be because it binds a single
     ; name to a single process
     #''TODO
     ]
    )
  )

#;(define-syntax (spec-no stx)
  ; bare name does not work as desired
  (syntax-parse stx
    #:datum-literals (.uses .inputs .outputs)  ; measures can bubble up anywhere because spec is not required to be atomic...
    [(_ (~alt name  ; black box
              (name symbolic-inputs ...)  ; process with export time constraints TODO prov how?
              ; the problme is that black boxes might also have export time constraints since some higher level invariant
              ; is supposedly all that matters, such as ACSF, to actualize it (.vars final-volume) is still there...
              ()
               ))]

    )
  )



(define-syntax (invariant stx)
  #'"TODO")

(define-syntax (.steps stx)
  (raise-syntax-error 'wrong-context ".steps was used in the wrong context")
  )

(define-for-syntax (bind-properties stx . property-alist)
  (let ([next (cdr property-alist)]
        [key (caar property-alist)]
        [value (cdar property-alist)])
    (if (null? next)
        stx
        (syntax-property (bind-properties next) key value)))
  )

(define-syntax (spec stx)
  (syntax-parse stx
    #:disable-colon-notation
    #:datum-literals (:> make
                      *: >^> measure
                      :* v> actualize ; does >v> even really exist? lots-of-salt v some-salt ? type preserving but not amount?
                      ;>v>' modify  ; ' could expand to include % conservative subsetting of amount, for composite black boxes
                      ; leaving modify out since it technically is a specialization of make and the functionality
                      ; that we want is the ability to continue to refer to the mouse by its original name while
                      ; carrying the information that it has been modified

                      black-box participant

                      order
                      .uses
                      .vars
                      .config-vars
                      .inputs
                      .outputs
                      .symret
                      .steps

                      )
    #:local-conventions ([name id]
                         ;[aspect id]
                         [docstring string]
                         [input id]
                         [constrained-input id]
                         [var id]
                         [step sc-step-ref]  ; FIXME this appraoch means that (.steps 1 2 3 4) fails by becoming part of body
                         ; which is _super_ confusing :/
                         ; should probably find a way to fail on that either by binding .steps to syntax or something else
                         [import id]
                         [aspect id]
                         [aspect* expr]  ; TODO
                         [oper id]
                         )
    [(_ (~or (black-box specific-name parent-type) (participant specific-name parent-type))
        (~optional docstring)
        ; complement ...
        ; known complement (causal)
        ; unknown complement (causal)
        ; TODO merging of local namespaces :x
        body ...
        )
     ; FIXME what is the distinction between a make spec and a black-box spec?
     ; ANSWER? the black box is just a bunch of names with some structure that
     ; can be reused, and ephys right gives the definition, and while we could
     ; in theory say how to make it after the fact, the same is definitely not
     ; true of a cell, or at least, the use of the term make instead of define
     ; certainly seems to confuse matters. However, black boxes serve as names
     ; awaiting definition, in protc/stric we use them to keep terms that lack
     ; an operational definition, they could be being ignored, or like physics
     ; they can be treated as hypotheses that need to be tested
     #:with export-stx #'((.executor)
                          (.type . black-box)
                          (.name . specific-name)
                          (.parent . parent-type)
                          ;(.parts parts ...)  ; TODO in the body...
                          (.docstring . (~? docstring ""))
                          (.inputs)
                          (.outputs)
                          (.vars)
                          (.measures)
                          (.steps)
                          (other body ...))
     #:with name-stx (bind-properties (format-id #'specific-name
                                                #:source #'specific-name
                                                "~a-stx" (syntax-e #'specific-name))
                                      (syntax->datum #'export-stx)
                                      )
     #:with name-ast (format-id #'specific-name
                                #:source #'specific-name
                                "~a-ast" (syntax-e #'specific-name))
     #'(begin
         #;(define-syntax name-stx
           (syntax-property
            (syntax-property
             (syntax-property #'export-stx 'name #'specific-name)
             'spec (list))
            'impl (list)))
         (define name-stx export-stx)
         (define name-ast
           '(data export-stx))
         (define specific-name #'#''(specific-name))  ; TODO black-box struct (already sort of done elsewhere)?
         )
     ]
    [(_ (~or (make name) (:> name)) ; make binds the output name as a being/symbol
        ; this approach has the drawback that (make name) is now the only way to refer to this process?
        ; false, that is where impl comes in, but how do we deal with the 1000 different ways to spec
        ; (measure mouse is) ? most of the time we are not going to be 'making' mice... because we
        ; since that is essentially (begin (+ male-mouse female-mouse food water territory) (wait)) -> mice
        (~optional docstring)
        (~optional (.uses import ...))  ; subProtocolOf ???
        (~optional (.vars var ...))  
        ;(~optional (.inputs inputs ...))
        (~optional (.inputs (~or input [oper constrained-input aspect* ...]) ...))
        ;(.outputs outputs ...)  ; technically these should be extra-outputs?
        (~optional (.steps step ...))
        body ...
        ;return-being  ; all we have is the name, the defining measures probably should return implicitly...
        )
     ; have to use format-id to get these new identifiers bound correctly for some reason
     #:with name-stx (format-id #'name
                                #:source #'name
                                "~a-stx" (syntax-e #'name))
     #:with name-ast (format-id #'name
                                #:source #'name
                                "~a-ast" (syntax-e #'name))
     #:with export-stx #'((.executor)
                          (.type . make)
                          (.name . name)
                          (.docstring . (~? docstring ""))
                          (.inputs (~? (~@ input ...)) (~? (~@ constrained-input ...)))
                          (.outputs name)
                          (.vars (~? (~@ var ...)))  ; TODO these need to be requested before export to pdf
                          ;(.measures (~? (constrained-input aspect* ...)) ...)
                          (.steps (~? (~@ step ...)))  ; FIXME if you see ?: attribute contains non-list value it is because you missed ~?
                          ; the debug message is totally useless :/ took me 3 hours to figure out how to debug it properly
                          (other body ...)
                     )
     #:with name-binding (let* ([-name #'name]
                                [-name-stx #'name-stx]
                                
                                #;[slv (syntax-local-value (format-id #'name "~a-stx" (syntax-e #'name))
                                                         ; it can't be that this is being shadowed can it?!
                                                         ;-name-stx
                                                         #;(λ () #''(AAAAAAAAAAAA name)))]
                                #;[argh slv
                                 ;(debug-repl)
                                 #;(pretty-print slv)
                                 #;(if (identifier-binding -name)
                                     #;#''(TODO agument the thing, but syntax-local-value doesn't seem to work?!)
                                     (begin
                                       (pretty-print -name)
                                       slv  ; RIGHT?!?!
                                       #;(syntax-local-value #'name))
                                     ;(syntax-local-value -name-stx)  ; why u no bound!?
                                     #'wat)])
                           (if (identifier-binding #'name)
                               (if (identifier-binding #'name-stx)
                                   ;#''(set! the name and name-stx)
                                   #`(begin
                                       ; TODO check the state of the stx object first...
                                       ; should not overwrite an impl with a spec
                                       ; TODO how to update the existing syntax properties
                                       #;#,(let ([slv (syntax-local-value #'name-stx)]
                                               [-name-stx #'name-stx]
                                               [-name #'name]
                                               )
                                           (syntax-property #'name-stx 'mod "WTF m8")
                                           (println (syntax-property #'name-stx 'fuck))
                                           (println (syntax-property #'name-stx 'mod))
                                         )
                                       ;(define-syntax name-stx (syntax-property #'name-stx 'spec #'export-stx))
                                       (set! name-ast '(data export-stx))
                                       (set! name specification-phase)
                                       )
                                   #'(define name "Name already bound to non-spec value. Will not overwrite.")
                                   #;(raise-syntax-error 'already-boud "Name already bound" #'name))
                               #'(begin
                                   (define name-stx #'export-stx)  ; this is ok except for the body bit...
                                   (define name-ast '(data export-stx))
                                   (define name specification-phase))
                                    )
                           #;
                           #`(if-defined name
                                         ;#,argh ;'(internal screaming)
                                         #,(if (identifier-binding -name #;3)
                                               ; i have no idea why this fails now :/
                                               ; answer! it fails because "thing" is the root term you idiot
                                               ; and doesn't have a -name-stx (maniacal laughter)
                                               (syntax-local-value -name-stx #;(format-id #'name "~a-stx" (syntax-e #'name)))
                                               ''(u wot m8, seriously?)
                                               )
                                         (define name specification-phase)))
 
     #;(pretty-write (list 'ct:
                           (attribute name)
                           (attribute import)
                           (attribute var)
                           (attribute input)
                           (attribute step)
                           (attribute body)
                           ))
     (let ([out 
            #'(begin
                (define (specification-phase)
                  ; we have to be able to talk about these before they are bound ...
                  'make
                  (~? (define-values (input ... constrained-input ...)
                            ;; "actualize"  ; turns out racket does have function types, they just don't tell you ^_^
                            (bind/symbol->being input ... constrained-input ...)) )
                  body ...  ; free variables work naturally here, but the body will be invisible
                  ; body.stuff ...
                  ; to export unless we can look inside, because we can't ask users to
                  ; communicate directly
                  ; NOTE we will need to lift vars from any specs called in here
                  ; NOTE we will also need to orchestrate passing those in ...
                  ; symbolic outputs don't have to be dealt with here because their own sections will be called
                  ; we just have to orchestra passing in variables ...
                  (define (~? (symbolic-input-phase var ...) (symbolic-input-phase))
                    (define (*start-execution*)
                      
                      ; TODO validate inputs step needs options auto, on, skip or something like that
                      (~? (validate/being input ... constrained-input ...))
                      (validate/being name))
                    *start-execution*)
                  symbolic-input-phase)
                name-binding
                #;(define-syntax name-stx
                  #'export-stx)
                #;(define name-ast  ; have to use format-id to get this bound correctly for some reason
                  '(data
                    export-stx))
                )])
       #;(pretty-print (syntax->datum out))
       out)
     ]
    [(_ (~or (measure name ...+ aspect) (>^> name ...+ aspect) (*: name ...+ aspect))
        ; FIXME why does (spec (black-box a b c d) "asdf") work here?!??!!
        ; measure binds a
        ; aspec-predicate-category
        (~optional (.uses import ...))  ; the reason we do this is to keep the relevant names under control
        (~optional docstring)
        (~optional (.vars var ...))  
        (~optional (.config-vars cvar ...))  ; TODO naming ...
        (~optional (.inputs (~or input [oper constrained-input aspect* ...])...)) ; FIXME
        ;(.outputs outputs ...)    ; unused?
        (~optional (.steps step ...))
        (~optional (.symret predicate))
        ; NOTE: we may not need this, it could be _either_ an aspect or a predicate
        ; where predicates represent a simple threshold function
        ; but then we also have category functions aka classifiers ...
        ; we could detect on predicate? :aspect or aspect and category% (% is nice because class ...)
        ; TODO naming, this is really a type restriction ... also (values ...)
        ; but... how to spec data structure and binding for more complex cases
        ; also contracts...

        body ...
        #;return-symbolic)  ; FIXME I think this is wrong...
     #:with name:aspect (format-id #'aspect  ; NOTE #'(name ... aspect) does not work :/
                                   #:source #'aspect
                                   "~a:~a"
                                   (string-join (map symbol->string
                                                     (syntax->datum #'(name ...))) "-")
                                   (syntax-e #'aspect))
     #:with name:aspect-ast (format-id #'aspect
                                       #:source #'aspect
                                       "~a-ast" (syntax-e #'name:aspect))
     ;(println (syntax-e #'name:aspect))
     ;(println (syntax-e #'name:aspect-ast))
     #:with export-stx #'((.executor)
                          (.type . measure)
                          (.name . name:aspect)
                          (.docstring . (~? docstring ""))
                          ;(.inputs (~? (name ... inputs ... constrained-input ...) (name ...)))
                          (.inputs name ... (~? input) ... (~? constrained-input) ...)
                          (.outputs name ...)  ; hopefully ...
                          (.vars (~? var) ...)  ; TODO these need to be requested before export to pdf
                          (.measures aspect (~? (constrained-input aspect* ...)) ...)  ; FIXME aspect black-box binding
                          (.steps (~? step) ...)
                          (other body ...))
     (let ([out 
            #'(begin
                (define (specification-phase)  ; TODO maybe use parameterization to pass in executors and runtime info?
                  'measure  ; when unquoted failes as expected
                  (~? (define-values (name ... input ... constrained-input ...)
                            (bind/symbol->being name ... input ... constrained-input ...))
                          (define (name ...) (bind/symbol->being name ...)))
                  (~? (define-values (cvar ...) ; black-box-parts are beings too!
                        (bind/symbol->being cvar ...)))
                  body ...
                  (define (~? (symbolic-input-phase var ...) (symbolic-input-phase))
                    (define (*start-execution*)
                      (~? (validate/being input ... constrained-input ...))
                      (validate/being name ...)  ; NOTE thing vs putative-thing
                      ; when we try to bind a name and fail because the criteria are
                      ; not met, that is a putative name, we need some way to prevent
                      ; failed defining measures from binding to the name/type at runtime
                      ; for the purposes of generics this becomes a bit confusing because
                      ; we can technically call anything a mouse that we want to, including
                      ; a rat, a cat, or a hat, therefore we need to make sure that when
                      ; input types are enforeced, that measures that are used to define a
                      ; thing are correctly tagged as putative

                      ; TODO paramerize ... 
                      ; FIXME for certain executors and implementations
                      ; the overseer may never actually get the result
                      ; we need a way to indicate this ... (if impl-says-to-save-this do-it just-note-it)
                      (define result
                        (let* ([protc:value (runtime-read name:aspect)]
                               ; TODO .symret may propagate up to runtime-read
                               ; and then we can deal with failure there instead of
                               ; after the fact, though having seen users struggle with
                               ; type systems it may be something for protc/strict not protc/base or protc
                               [correct (~? (predicate protc:value) #t)])
                          (struct/result protc:value
                                         ; aspect is implied? we have to defer because we may not know units
                                         (runtime-protocol name:aspect)
                                         (runtime-measure-name name:aspect)  ; in some cases this will be an aspect
                                         (runtime-executor)
                                         ; absolutely must pull time from the server
                                         (current-milliseconds))))
                      '(sign result)  ; TODO this needs to be decoupled, but runtime loggedin signing is a start...
                      result
                      )
                    *start-execution*)
                  symbolic-input-phase)
                (define name:aspect-stx
                  #'export-stx)
                (define name:aspect-ast  ; have to use format-id to get this bound correctly for some reason
                  '(data
                    export-stx))  
                (define name:aspect specification-phase))])
       #;(pretty-print (syntax->datum out))
       out)
     ]
    [(_ (~or (actualize name) (v> name) (:* name))  ; actualize binds the output name as a process which could have many being outputs
        (~optional docstring)
        ; FIXME aspect!?
        ;(~optional (.uses imports ...))
        (~optional (.vars vars ...))  
        (~optional (.inputs (~or input [oper constrained-input aspect ...]) ...))  
        (~optional (.outputs outputs ...))  
        body ...
        ;return-being  ; do we have this?
        )
     #:with name-ast (format-id #'name
                                #:source #'name
                                "~a-ast" (syntax-e #'name))
     #:with export-stx #'((.executor)
                          (.type . actualize)
                          (.name . name:aspect)
                          (.docstring . (~? docstring ""))
                          (.inputs name (~? input) ...)
                          (.outputs name)  ; hopefully ...
                          (.vars (~? vars) ...)  ; TODO these need to be requested before export to pdf
                          (.measures (constrained-input aspect ...) ...)
                          (.steps)
                          (other body ...)
                     )
     ; FIXME need a way to check this syntax here and then combine it with impl to form the real output
     ; we do also still want to allow people to view only the spec phase if they want
     (let ([out 
            #'(begin
                (define (specification-phase)  ; TODO maybe use parameterization to pass in executors and runtime info?
                  'actualize
                  (~? (define-values (name input ... constrained-input ...)
                        (bind/symbol->being name input ... constrained-input ...))
                      (define (name) (bind/symbol->being name)))
                  #;(~? (define-values (cvars ...) (bind/symbol->being cvars ...)))
                  body ...
                  (define (~? (symbolic-input-phase vars ...) (symbolic-input-phase))
                    (define (*start-execution*)
                      (~? (validate/being input ... constrained-input ...))
                      (validate/being name)  ; NOTE thing vs putative-thing
                      ; when we try to bind a name and fail because the criteria are
                      ; not met, that is a putative name, we need some way to prevent
                      ; failed defining measures from binding to the name/type at runtime
                      ; for the purposes of generics this becomes a bit confusing because
                      ; we can technically call anything a mouse that we want to, including
                      ; a rat, a cat, or a hat, therefore we need to make sure that when
                      ; input types are enforeced, that measures that are used to define a
                      ; thing are correctly tagged as putative

                      ; TODO paramerize ... 
                      (define result 
                        (struct/result (runtime-read name)
                                       ; aspect is implied? we have to defer because we may not know units
                                       (runtime-protocol name)
                                       (runtime-measure-name name)  ; in some cases this will be an aspect
                                       (runtime-executor)
                                       ; absolutely must pull time from the server
                                       (current-milliseconds)))
                      '(sign result)  ; TODO this needs to be decoupled, but runtime loggedin signing is a start...
                      result
                      )
                    *start-execution*)
                  symbolic-input-phase)
                (define-syntax name-stx
                  #'export-stx)
                (define name-ast  ; have to use format-id to get this bound correctly for some reason
                  '(data
                    export-stx))  
                (define name specification-phase))])
       #;(pretty-print (syntax->datum out))
       out)
     ]  
    [(_ (order name)
        ;inputs and outputs inferred
        (~optional (.vars var ...))
        body ...)
     #:with name-spec (format-id #'name
                                 #:source #'name
                                 "~a-spec"
                                 (syntax-e #'name))
     #'(define name-spec '(body ...))  ; TODO need to make it require vars etc... plus a way if-defined...
     ]
    ))

; we want to be able to start from either spec or impl...

(define-syntax (impl stx)
  ; TODO impl may partially apply (or wholy apply) a spec and defer the rest until 'real' run time
  ; aka impl-process
  (syntax-parse stx
    #:disable-colon-notation
    #:local-conventions ([spec-name id]
                         [impl-name id]
                         [docstring string])
    #:datum-literals (order .inputs)
    #;[(_ name body ...)
       #''TODO]
    #;[(_ (name) body ...)
       #''TODO]
    [(_ (order impl-name)  ; FIXME or is it really (protocol name) and then in the body (order statement ...)
        body ...)
     #'(define impl-name (list body ...))  ; TODO need to deal with if-defined
     ]
    [(_ (spec-name (~optional impl-name))
        (~optional docstring)
        (~optional (.inputs inputs ...))  ; implementation specific inputs i.e. ones that should be results invariant
        body ...
        )
     #'(begin

         )
     ]
    )
  )

#;(define-syntax (measure stx)
  (syntax-parse stx
    [(_ thing aspect ...)
     #''TODO])
  )

#;
(module+ test-spec-1
  (define-syntax (spec-1 stx)
    (syntax-parse stx
      ; constrained-by ; compile time / write time, (result thing) just needs a way to bind proveancne, which is 2 things, measure function name and execution trace identifier
      ;    comes from body
      ; has-symbolic-input
      ;    these are .vars in the current thinking
      ; has-input
      ;    these are .inputs in the current thinking
      ; has-output
      ; has-symbolic output

      ; the executor function needs to define symbol->being and being->symbol or maybe that goes somehwere ehse?
      #:datum-literals (.uses .inputs .outputs .vars .measures)
      #:local-conventions ([body sc-protc-body]  ; TODO allow these to be defined dynamically
                           [inputs sc-prtoc-input]
                           [outputs sc-protc-output]
                           [required-symbolic-inputs id]  ; number/literal, number + unit (literalis require translation)
                           [required-symbolic-outputs id]  ; number/literal, number + unit (literalis require translation)
                           [required-symbolic-inputs sc-id+unit]
                           [required-symbolic-outputs sc-id+unit]
                           [name id]
                           [steps sc-step-ref]
                           [doc string]
                           )
      [(_ (name)  ; FIXME this gives bad syntax error even though this is defined ... :/
          (~optional (~seq #:executor executor) #:defaults ([executor #'"human"]))
          (.uses imports ...)
          body ...)
       #''TODO]
      [(_ (name required-symbolic-inputs ...)  ; FIXME this gives bad syntax error even though this is defined ... :/
          (~optional (~seq #:executor executor) #:defaults ([executor #'"human"]))
          (~optional (.uses imports ...))
          (~optional (.inputs inputs ...))
          (~optional (.outputs outputs ...))
          body ...)
       #''TODO]
      [(_ (~seq #:name name)  ; FIXME this gives bad syntax error even though this is defined ... :/
          (~optional (~seq #:executor executor) #:defaults ([executor #'"human"]))
          (.uses imports ...)
          (.inputs inputs ...)
          (.outputs outputs ...)
          (.vars required-symbolic-inputs ...)
          )
       (begin #''TODO)]
      [(_ (~seq #:name name)  ; FIXME this gives bad syntax error even though this is defined ... :/
          (~optional (~seq #:executor executor) #:defaults ([executor #'"human"]))
          (.inputs inputs ...)
          (.outputs outputs ...)
          (.vars required-symbolic-inputs ...)
          )
       (begin #''TODO)]
      [(_ (~seq #:name name)
          (~optional (~seq #:executor executor) #:defaults ([executor #'"human"]))
          (.inputs inputs ...)
          (.measures required-symbolic-outputs ...))
       (begin #''TODO)] 
      [(_ (~seq #:name name)  ; these are our normal functions the body can be anything
          (~optional (~seq #:executor executor) #:defaults ([executor #'"human"]))
          (.vars required-symbolic-inputs ...)
          (.measures required-symbolic-outputs ...))
       (begin #''TODO)] 
      [(_ (name required-symbolic-inputs ...)
          (~optional (.uses imports ...))
          (.inputs inputs ...)
          (.outputs outputs ...)
          (.measures required-symbolic-outputs ...)  ; FIXME this needs to be replaced with aspect-input pairs?
          body ...)
       #:with (bout ...) (datum->syntax this-syntax (flatten (syntax->datum #'(body.outputs ...))))
       #'(begin
           (define (specification-phase)
             ; FIXME this doesn't work... it is missing the impl sections
             ; this takes no inputs because they are extracted from the body
             ; this defines symbol->being and being->symbol possibly via require through the executor
             ; you could imagine using using asymmetric crypto to certify that the measurement devices have not been tampered with
             ; but without control of the hardware that is virtually impossible, we can only trust any _individual_ measurement device (which means that you have to distrust all of them...)

             ; TODO the symbols are defined up here for inputs outputs and vars
             ; they are 'bound' to their referent by the referent data
             body.invariant-binding-form ...  ; these need to expand to functions that fill in the missing info
             ; body.parameters ...
             body.validate/being ...   ; we only have to validate ours, all the other definitions will do theirs too
             body.telos ...  ; individual things can also have their own?
             (define (symbolic-input-phase required-symbolic-inputs ... body.required-symbolic-inputs ...)
               (define (*start-execution*)  ; input-phase
                 ; the symbolic representation of the protocol takes no arguments because those inputs
                 (define-values (inputs ... body.inputs ...)  ; this gives us all the named things we have to deal with
                   ;; "actualize"  ; turns out racket does have function types, they just don't tell you ^_^
                   (bind/symbol->being inputs ... body.inputs ...))
                 ; body.steps  ; TODO setup/prerecs go here when we are compiling the human readable version? and then execution goes functionally in symbolic outputs define values?
                 (define-values (required-symbolic-outputs ... body.required-symbolic-outputs ...)
                   ;; "measure"
                   (being->symbol required-symbolic-outputs ... body.required-symbolic-outputs ...))
                 ; TODO ? it seems that lifting body.outputs to here breaks ordering rules...
                 ; so we need to include a way to order evaluation
                 (validate/being outputs ... bout ...) ; this uses the implicit/internal measures on those outputs
                 (values required-symbolic-outputs ... body.required-symbolic-outputs ...))
               *start-execution*)
             symbolic-input-phase)

           (define name-ast  ; have to use format-id to get this bound correctly for some reason
             '(data
               ((.executor executor)
                (.name name)
                (.inputs (inputs ...))
                (.outputs (outputs ...))
                (.vars (required-symbolic-inputs ...))  ; TODO these need to be requested before export to pdf
                (.measures (required-symbolic-outputs ...)))))  
           (define name specification-phase))
       ]
      [(_ name
          (~optional (.identifier ident))  ; allow mapping of names specced locally, we can provide (spec-ident (~seq local global) ...) too
          ; and can then pull in the expected parts from the ontology
          body ...)
       #:with name-predicate (format-id #'name #:source #'name "~a?" (syntax-e #'name))
       #'(begin
           (define (name-predicate expected-results)  ; TODO results/result structure
             #f  ; TODO (for/and value-checkers expected-results)
             )
           )
       ]
      [(_ (~seq #:name name)  ; the big block has to come last or it will eat all the  rest
          (~optional (~seq #:executor executor) #:defaults ([executor #'"human"]))  ; is executor really just a one of? I think so, parallelism ...
          (~optional doc)  ; FIXME this might be undesireable...
          (.uses imports ...)
          (.inputs inputs ...)
          (.outputs outputs ...)
          (.vars required-symbolic-inputs ...)
          (.measures required-symbolic-outputs ...)  ; aka symret
          ;(.steps steps:sc-step-eval ...)  ; FIXME hack
          (.steps steps ...)  ; FIXME hack
          ; TODO allow these to be in any order? also note that this is the non-atomic ordering
          body ...)
       #:with name-ast (format-id #'name
                                  #:source #'name
                                  "~a-ast" (syntax-e #'name))
       #:with name-stx (format-id #'name
                                  #:source #'name
                                  "~a-stx" (syntax-e #'name))
       #:with (bout ...) (datum->syntax this-syntax (flatten (syntax->datum #'(body.outputs ...))))
       #:with docstringf #'(λ (inputs ...) (format (~? doc "") inputs ...))
       #:with docstring (if (attribute doc)
                            (if (attribute inputs)
                                (apply format (syntax-e #'doc) (syntax->datum #'(inputs ...))) ; FIXME we want to format this via args
                                #'doc)
                            (datum->syntax #'name (format "do ~a" #'name)))

       #;(let ([names (syntax->list #'((~? steps.name) ...))])
           (unless (null? names)
             (println names)
             (println (map
                       (compose syntax-local-value
                                (λ (name) (format-id name #:source name "~a-stx" (syntax-e name))))
                       ;syntax-local-value
                       names))))
       #:with (subprotocols ...) (let ([names (syntax->list #'((~? steps.name) ...))])
                                   (map
                                    (compose syntax-local-value
                                             (λ (name) (format-id name #:source name "~a-stx" (syntax-e name))))
                                    names))

       #'(begin
           (define (specification-phase)
             ; this takes no inputs because they are extracted from the body
             ; this defines symbol->being and being->symbol possibly via require through the executor
             ; you could imagine using using asymmetric crypto to certify that the measurement devices have not been tampered with
             ; but without control of the hardware that is virtually impossible, we can only trust any _individual_ measurement device (which means that you have to distrust all of them...)

             ; TODO the symbols are defined up here for inputs outputs and vars
             ; they are 'bound' to their referent by the referent data
             body.invariant-binding-form ...  ; these need to expand to functions that fill in the missing info
             ; body.parameters ...
             body.validate/being ...   ; we only have to validate ours, all the other definitions will do theirs too
             body.telos ...  ; individual things can also have their own?
             ; FIXME TODO: how to lift .uses!
             (define (symbolic-input-phase required-symbolic-inputs ... body.required-symbolic-inputs ...)
               (define (*start-execution*)  ; input-phase
                 ; the symbolic representation of the protocol takes no arguments because those inputs
                 (define-values (inputs ... body.inputs ...)  ; this gives us all the named things we have to deal with
                   ;; "actualize"  ; turns out racket does have function types, they just don't tell you ^_^
                   (bind/symbol->being inputs ... body.inputs ...))
                 ; body.steps  ; TODO setup/prerecs go here when we are compiling the human readable version? and then execution goes functionally in symbolic outputs define values?
                 (define-values (required-symbolic-outputs ... body.required-symbolic-outputs ...)
                   ;; "measure"
                   (being->symbol required-symbolic-outputs ... body.required-symbolic-outputs ...))
                 ; TODO ? it seems that lifting body.outputs to here breaks ordering rules...
                 ; so we need to include a way to order evaluation
                 (validate/being outputs ... bout ...) ; this uses the implicit/internal measures on those outputs
                 (values required-symbolic-outputs ... body.required-symbolic-outputs ...))
               *start-execution*)
             symbolic-input-phase)

           ; FIXME use syntax-local-value here, vastly preferable
           ; keeps things safe at compile time
           (define-syntax name-stx
             #'((.executor . executor)
                (.name . name)
                (.docstring . docstring)
                (.docstringf . docstringf)
                (.inputs inputs ...)
                (.outputs outputs ...)
                (.vars required-symbolic-inputs ...)  ; TODO these need to be requested before export to pdf
                (.measures required-symbolic-outputs ...)
                (.steps steps.instruction ...) ; TODO pull in the asts at compile time
                (.subprotocols subprotocols ...)
                (other body ...)
                ))
           (define name-ast  ; have to use format-id to get this bound correctly for some reason
             '(data
               ((.executor . executor)
                (.name . name)
                (.docstring . docstring)
                (.docstringf . docstringf)
                (.inputs inputs ...)
                (.outputs outputs ...)
                (.vars required-symbolic-inputs ...)  ; TODO these need to be requested before export to pdf
                (.measures required-symbolic-outputs ...)
                (.steps (~? steps.instruction) ...) ; TODO pull in the asts at compile time
                (.subprotocols subprotocols ...)
                (other body ...)
                )))  
           (define name specification-phase))]
      ))

  (spec-1 #:name my-protocol
          (.uses)  ; TODO nothing...
          (.inputs mouse vibratome cut-buffer)
          (.outputs brain-slice)  ; TODO count
          (.vars anesthetic-dose slice-thickness)  ; other vars could be defined in body and lifted up?
          (.measures time-to-cell-death number-of-cells)  ; null for the top level protocol we can lift other ones
          (.steps "hello!?")
          "this is a body"
          ; the body of a spec is just here to bind thing aspect var
          ; the implementation is where the steps actually go
          ; do implementations have symbolic inputs? I don't think they do...
          )
  (spec-1 #:name spike?
          "measure whether ~a spikes"
          (.uses)
          (.inputs cell)
          (.outputs)
          (.vars)
          (.measures)
          (.steps
           "in current clamp mode"
           "watch the voltage trace"
           "if there is a spike there will be a small deflection"))

  (spec-1 #:name EPSP?
          "measure whether ~a produces an EPSP"
          (.uses)  ; cell membrane potential voltage-trace
          (.inputs cell)
          (.outputs)
          (.vars)
          (.measures)
          (.steps
           "in current clamp mode"
           "watch the voltage trace"
           "there will be an inward deflection")
          )

  (spec-1 #:name projects-a-b?  ; FIXME these should probably be impl??
          "measure whether ~a projects to ~a"  ; debug messages are so bad here, this is why we don't use strings kids
          (.uses)
          (.inputs cell-a cell-b)
          (.outputs)
          (.vars)
          (.measures)
          (.steps
           (spike? cell-a)
           (EPSP? cell-b)
           ; FIXME allow (and (spike? cell-a) (EPSP? cell-b))
           "(define projects-a-b (and (spike? cell-a) (EPSP? cell-b))"  ; FIXME actual
           ))

  (spec-1 #:name loose-patch
          "create a low resistance seal between ~a and ~a"
          (.uses)
          (.inputs pipette cell)
          (.outputs)
          (.vars)
          (.measures)
          (.steps
           "use the controller to move the pipette tip to touch the cell"
           "once they are touching wait"
           ))

  (spec-1 #:name whole-cell-patch
          "create a high resistance seal between ~a and the interior of ~a"
          (.uses)
          (.inputs pipette cell)
          (.outputs)
          (.vars)
          (.measures)
          (.steps
           "use the controller to move the pipette tip to touch the cell"
           "once they are touching wait"
           "apply a slight negative pressure until a gia ohm seal forms"
           ))

  (spec-1 #:name test-connected-pair
          "test whether ~a is connected to ~a using two of ~a"
          (.uses)
          (.inputs #;brain-slice
                   cell-a
                   cell-b
                   patch-pipette  ; FIXME technicalyy need more than one :/
                   )
          (.outputs)
          (.vars command-potential
                 pipette-resistance
                 pulse-current
                 pulse-duration)
          (.measures spike? EPSP? projects-a-b?)  ; FIXME xml escape!
          (.steps
           ;"cell-b should already be whole-cell-patch"
           (whole-cell-patch patch-pipette cell-b)  ; FIXME need disambig
           ;"loose-patch cell-a"
           (loose-patch patch-pipette cell-a)
           "stimulate cell-a to evoke 1 spike (usually 5ms and 0.1-1 nA)"
           (projects-a-b? #;patch-pipette cell-a cell-b)
           "measure EPSP? (sp 3)"
           "compute projects-a-b? (sp 4)"
           )
          
          )

  (define protc-for-export
    (list test-connected-pair-ast))
  (provide protc-for-export)  ; TODO compiled protc modules should export this automatically
  (export test-connected-pair 'html 'pdf)
)

(spec (black-box thing thing)
      "the root of all things")

(spec (black-box sub-thing thing))
(spec (make something-else-no-steps)
      "make thing")
(spec (make something-else)
      (.steps "1" "2" "3" "4")
      "make thing")

#;
(spec (make yet-another-thing)
      (.steps 1 2 3 4)
      "failes as expected")

(spec (make thing)
      (.inputs hello world)
      "instructions")

#;
(spec (actualize pulse)
        (.vars duration
               duration-units
               step-size
               step-units)
        (.inputs [setting pulse-maker
                          ([step-units step-size]
                           [duration-units duration])]
                 ....))

#;(spec (make loose-patched-cell)
        (.inputs brain-slice
                 internal-solution
                 (: patch-pippette ([MOhms (range 6 10)])))  ; ERROR: don't know how to measure
        (part-of brain-slice cell)
        (put-a-in-b internal-solution patch-pippette)
        ;(.output (loose-patch cell patch-pipette))
        )

#;
(module+ test

  ;(spec (measure ok ya) "yes?")
  ;(spec (black-box should fail) "yes?")  ; was a typo of >v> for >^> >_<
  ;(spec (black-box wut) "stahp")

  

  ; at the implementation stage functions operate on inputs
  ; whereas at the spec stage functions operate on symbolic inputs
  (impl (inject thing)
        (.inputs thing)
        )
  (impl (injected-mouse)  ; i don't think this is an impl because spec is all that we need to refer to it symbolically?
        (.inputs mouse)
        (inject mouse))


  ; side note: expected execution time is a profiling result

  (spec (measure protocol execution-time)  ; (aspect (duration)) ?????
        ;(.inputs protocol)  ; ah, how to deal with occurents... isn't execution time an aspect?
        ; because of the structure time we can't actually implement a function that takes a process as an input
        ; because we really cannot close over the inputs, in a way it is even less satisfying than the cosmic ray closure issue
        ;(.uses protocol)  ; aka protocol?
        #;(measure protocol duration)  ; sort of redundant?
        )

  (define (time-at-term-input)
    (writeln "time will be recorded next time there is terminal input")
    (read-bytes 1)
    (current-milliseconds))
  (define (duration process)
    (read-bytes 1)  ; if it is called directly 
    (define start (time-at-term-input))
    ;(%#rw-eval process)
    (define stop (time-at-term-input))
    (- stop start))

  (impl (execution-time)  ;(measure duration)
        ;(.inputs process)  ; this is just a named process... at the symbolic level we want to write
        ;(.inputs stop-watch)
        (bind-aspect duration duration)  ; ... hrm
        ;(duration process)
        )

  (spec (make brain-slice);(my-protocol slice-thickness)
        (.vars slice-thickness)
        (.inputs mouse vibratome)
        ;(.outputs brain-slice)
        ;(.measures time-to-cell-dealth number-of-cells)  ; -> (values ...) at the end (measure-time-to-cell-death all-cells-in-slice) -> time-to-cell-death
        ; and if we have people put .measures in explicitly then at run time they return as an ... alist?
        ; we don't want to just return the last value every time ie (values m1 m2 m3 m4)
        ; closed loop...
        )

  (spec (measure brain-slice time-to-cell-death)  ; ok, this different... it is not the usual being aspect pairing...
        ; and the black box is _totally_ not obvious
        (.inputs)
        )

  (spec (make novars)
        "body")

  (spec (make thing);(protocol sinput1 sinput2 sinput3)
        (.vars sinput1 sinput2 sinput3)

        ; XXX NOTE this is somewhat misleading, because other export time inputs
        ; will be lifted as inputs to the function as well... but I guess
        ; for compositionality this might be clearer -- HOWEVER we will
        ; have to make sure naming collisions are resolved correctly
        ; because I could have an enclosed spec that has sinput1 as well
        ; FIXME the question is what is the USE of 'calling' a symbolic function
        ; in this way see example below
        "body"
        )
  ; this is really an imple that has no explicit spec
  ; probably should have protocol reserved for implementing ?
  (spec (measure thing aspect)
        "body")

  (spec (measure circuit potential)
        ;(.config-vars reference ground)  ; FIXME figure out how to deal with conflicts from has-part?
        ; FIXME these aren't really inputs, they are other things that need to be specialized?
        ; they aren't symbolic variables, they are... configuration variables ...
        '(= V (* I R))
        (has-part circuit reference ground)
        ; FIXME reference and ground are the same!
        (invariant (= potential (* I R)))
        #;"In order to measure the potential difference between two points
       one needs to have a known resistor, and inject a small amount of
       current into the system."

        "The above is incorrect. Measuring the potential between two points requires opening a
       circut where you know the resistance so that any current flowing through the circuit of
       interest can be measured. Because this creates a parallel circuit, it is important to use
       a large resistor so as not to induce a current drop in the circuit of interest. This means
       that you need to have a very sensitive voltimeter for measuring the potential drop across the
       known resistor."

        )
  (spec (measure cell membrane-potential))

  (spec (order protocol)
        (.vars si1 si2 si3)
        (if (< si1 si2)
            'order-1
            'order-2))
  (impl (order protocol)
        ; there is 
        )

  ;(spec (protocol si1 si2 [i1 i2 i3]))

  #;(spec (protocol number-of-batches si2 si3)
          ; this is not how we want bodies to work because
          ; we are not going to RUN these sections direclty
          ; the evaluation model for the export time information is different
          ; if I call (repeate protocol 100) i need to be able to distingish the
          ; (repeate-with-inputs) from (repeate) where an names are bound but
          ; I will reuse the same inputs... hrm... maybe simply marking things
          ; as consumed or not? we have our full transition states specced out already
          ; in 
          (spec (inner-protocol-0 final-volume)
                (.inputs [: volumetric-flask [volume final-volume]]
                         ; this is super akward and not at all it should be implemented since it is not composable
                         ; and the inner protocol spec doesn't actually depend on that information at all
                         [: solvent [> volume (* final-volume number-of-batches)]]
                         )
                )
          (spec (inner-protocol)
                (.inputs mouse)  ; (inputs [*: mouse weight])  TODO is there a simple way to allow [: mouse param1] [*: mouse mes2] in 1 exp?
                ;(inputs [*:* mouse (*: weight) (: age)])
                (.measures weight)
                (measure weight mouse)
                ; this is not a good example?
                )

          (inner-protocol)
          (inner-protocol-0)
          )

  #;(spec (process)
          ; I do not think we need (make asdf) (measure asdf) to be explicit?
          ; however (make-thing) and thing seem ambiguous, because thing implies measurement
          ; to _verify_ or define something, but then it is hard to define something based
          ; on the process that makes it, which was the original point
          "body")

  #;(spec thing
          ; no inputs in this spec? this seems to be pure symbols here aspects + values
          "definition")

  #;(spec thing-2
          ; this is also a technically valid way to specify something...?
          ; and I think it makes it clear that all the values that are present
          ; are invariant which is nice... this is where we could bind prov as well
          (my-make-thing))

  #;(spec (make thing)
          ; this breaks the (process) meaning, but it allows us to dissociate 
          )

  #;(spec some-salt  ; the issue how to nicely bind the name of the output when a function is run, without forcing the user to rename...
          (.vars amount)
          (.inputs [: salt [g amount]])
          #;(spec (_ amount)
                  (.inputs scale)  ; but this is now IMPL damn it
                  )
          )


  #;(spec (weigh-with-scale)
          (.inputs scale thing-to-weight ....)
          (.measures weight)
          (put-a-on-b thing-to-weight scale))
  #;(impl some-salt my-some-salt
          ; FIXME this doesn't seem like it is really an impl section
          (.uses [->: g weigh-with-scale])  ; (bind-aspect g weight-with-scale)
          ; we can intelligently bind an invariant that the scale must be able to weigh in grams
          )


  (require #;"export.rkt"
           "export-server.rkt"
           rdf/utils
           NIF-Ontology/rkt/ttl/methods
           )
  ; (take store 100)
  ;(require racket/pretty)
  ;(pretty-write my-protocol-ast)
  
  #;(export cell:membrane-potential 'html 'pdf)  ; FIXME why does this fail

  (parameterize ([runtime-executor (get-user)])
    ; TODO we will need a good way to manage
    ; parameterizing when protocols have multiple executors
    (((my-protocol) '100um '150mg/kg 'why-are-there-two?-because-body-has-a-fake-var))
    (parameterize ([runtime-executor "not tom"])
      (((thing:aspect))))
    )

  (let-values ([(name scrib) (protc->scribble my-protocol-ast #:user (get-user))])
    (scribble->html scrib #:name name)
    (scribble->tex scrib #:name name)
    (scribble->pdf scrib #:name name)

    ;(scribble->pdf scrib #:xelatex #t)
    )

  (export-file "direct-model.rkt")
  )
