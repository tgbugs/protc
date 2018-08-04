#lang racket/base
(require (for-syntax debug/repl))
(require ;scribble/srcdoc
         ;(for-doc scribble/base scribble/manual)
         protc/export
         ;protc/utils  ; this is private ...
         rdf/utils
         protc/private/utils
         (for-meta 2 syntax/parse racket/syntax racket/base)
         (for-syntax racket/base
                     racket/list
                     racket/string
                     racket/pretty
                     racket/syntax
                     syntax/parse
                     "syntax-classes.rkt"
                     "utils.rkt"))

(provide spec
         impl
         ;has-part
         ;part-of
         ;has-member
         define-relation
         define-aspect
         export
         invariant

         bind/symbol->being
         being->symbol
         validate/being
         )

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

(define (runtime-read . name:aspect)
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
(define (runtime-protocol . name:aspect) "current-protocol TODO")
(define (runtime-measure-name . name:aspect) '(get-current-impl name:aspect))
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
  ; FIXME -> telos, goal, expected state, required state, where (has-part? a b) needs to have a full measure spec and impl
  ; and we need to warn if it is missing

  ; FIXME this needs to be extended in the impl, but also needs a way to indicate a missing measure and actualize
  ; the statement of a 'fact' does not help us if we have no way to use those 'specs' as a point of reference for
  ; correctness, this is a telos!

  ; if has-part is used we have to know how to measure whether it is correct and how to to actualize it
  ; this is where genrics and dispatching on type/binding definitions per type is important...
  
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

;;; do notation for scoping and sequencing
;; do : ordered | unordered
;; ordered : practical | scientific  
;(define-syntax (do-unordered))
;(define-syntax (do-ordered))
;(define-syntax (do-practical))
;(define-syntax (do-scientific))

; TODO are there examples where there are practical unordered?
; e.g. buy all these things ... you have to do them but they
; aren't really scientific or unscientific
; VS buy this thing from exactly this vendor otherwise it wont work
; but that is not really scoping or ordering ... and for most of this
; stuff the (spec (get mouse)) and procurement chains (discussed elsewhere)
; will be handled via other mechanisms... so think on a better word...

(define-syntax (do stx)
  ; FIXME do conflates 2 things:
  ; 1. attribute reference scoping for bridging symbolic and being contexts
  ; 2. sequecing of actions
  ; it may be ok to conflate these, because in almost all cases the
  ; scoping will always acompany do (aka do-unordered)
  ; TODO consider counterexamples for the scoping rules

  ; FIXME we need a better abstraction than define-syntax for these
  ; than can accomodate multiple executors
  ; ALTERNATELY do is an executor independent construct for providing
  ; high level sequencing and execution rules in a way that can be
  ; specialized by an implementation using do-practical
  "do all these things, but not with any particular order requirements"
  (syntax-parse stx
    [(_ [unordered-action:sc-action ...]  ; TODO these need to be make/measure/actualize etc
        body ...)
     #:with (action-icipant-get-measure ...) #'(unordered-action.icipant ...) ;(format-id unordered-action.icipant ...)  ; TODO
     #'(begin
         ; retrieve all the action definitions (using syntax-local-value maybe?) and put them in .steps
         ; OR warn if the action has not been defined and put it in on the list of implicitly delegated

         (define (some-subprotocol unordered-action.vars ... ...)
           "TODO generate docstring from the retrieved definitions as well?"  ; TODO
           ;(define (action-icipant-get-measure unordered-action.measures ...) 'do 'stuff) ...
           (define (await-input)
             (define result  ; TODO this isn't quite right we have to bind the results back to their icipants
               (struct/result (runtime-read unordered-action.act-measure ... ... unordered-action.measure ... ...)
                              ; aspect is implied? we have to defer because we may not know units
                              (runtime-protocol unordered-action.act-measure ... ... unordered-action.measure ... ...)
                              (runtime-measure-name unordered-action.act-measure ... ... unordered-action.measure ... ...)
                              (runtime-executor unordered-action.act-measure ... ... unordered-action.measure ... ...)
                              ; absolutely must pull time from the server
                              (current-milliseconds)))
             ; for unordered prepare continuations for all awaiting measures
             ; and then wait on all of them, or something like that
             body ...
             )))]))

(define-syntax (do* stx)
  ; FIXME * is confusing because I use it by lisp convetion to imply that something should be ordered
  ; but also in the original protc convention to mean 'in a left to right input/output start/end, the
  ; asterisk tells you where the world and uncertainty is' maybe we can get some better symbol for that...
  ; ~ maybe? not distinct enough

  ; also FIXME: do* is an imperative version of sequential application
  ; it seems plausible that do* could actually be do-practical because
  ; the scientific ordering dependencies should be expressed functionally
  ; I'm fairly certain that this is true, because if there is _any_
  ; path dependance in the process then even things like
  ; ((measure skin redness) ((wait (quantity 10 (units 'seconds))) (slap skin)))
  ; can be expressed functionally, if we don't for people to write this way
  ; (which is probably a good idea, heh) then behind the scenes it is probably
  ; a good idea to translate do* into functional form...
  (syntax-parse stx
    [(_ [ordered-action:sc-action ...]
        body ...)
     #'(begin
         ; 1. make the executor format
         ; 2. make the overseer function
         ; TODO handle nesting of these
         )]))

(define-syntax (do-practical* stx)
  (syntax-parse stx
    [(_ [ordered-action:sc-action ...]
        body ...)
     #'(begin
         ; 1. make the executor format
         ; 2. make the overseer function
         ; TODO handle nesting of these
         )]))

(module+ test
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

  (spec (measure parent has-part?)  ; vs (measure has-part?) (.inputs child)
        (.inputs child)
        (.symret boolean?))

  ; TODO need to modify #%top so that when we are inside an executor body section has-part?
  ; and friends can be used like normal functions, they are aspects of subsets of larger black boxes
  ; there is nothing that limits aspects to being single arity but still a TODO on how to extend

  ; FIXME FIXME this should be detecting the existing has-part? defined above!!!!
  (impl (has-part?)
        (.executor human)
        (and (observe? parent)  ; this is mostly implicit observe? -> (true? (*observe parent))
             (observe? child)
             ; oh look, just follow the pattern if you don't know what to do!
             ; actualize* takes a thing and an aspect which means that you need both!
             ; and that way the thing you are actualizing/measuring will always be present
             ; for symbolic manipulation and examination!
             ; implicit when? or how to communicate this...

             ; this is perfect, order and order* used this way allow use to
             ; explictily create the scope in which a measurement is going to
             ; be made and do all the symbolic things that we want with them
             ; afterward in the body, the instructions will translate to
             ; "displace the parent in x, y, z space and record how much you moved it"
             (do* [(:* parent displacement vector3)
                      (*: child displacement vector3)]
                  (= (: parent displacement)
                     (: child displacement)))

             ; more redable version
             ; note that sticking the vecotr type on the end is adding an explicit data type
             ; which is impl... the spec could have just displacement and then we need a way
             ; to allow the implementation to specify that that is displacement in
             ; (param:dimensions (param:unit 'meters)
             ;                   (param:unit 'meters)
             ;                   (param:unit 'meters))
             ; (dim 'meter 'meter 'meter) (dim-n 3 'meter) vector3 'meter somehow...
             (do* [(actualize parent displacement vector3)
                   (measure child displacement vector3)]
                  (= (: parent displacement)
                     (: child displacement)))
             #;
             (do*
              ; actually I think having the scope helps a whole lot
              ; because it says that the values are those that are in
              ; the context of _this_ action, there could be many actions
              (:* parent displacement vector3)
              (*: child displacement vector3)
              (= (: parent displacement) (: child displacement)))
             ; basically begin
             #; ; old version, which sort of works but is probably overly verbose
             (do* ([action (:* parent displacement vector3)]
                   ; we could define move* as (:* parent displacement vector3) ...
                   ; and that would allow us to propagate the fact that when you move something
                   ; there is some displacement, whether you actually measured it or not will only
                   ; matter if you do something with it down the line
                   [outcome (*: child displacement vector3)])
              #;([action (move* parent vector3)]  ; vs move:distance*
                   [outcome (*move:vector3 child)])

                  (= (: action vector3) (: outcome vector3))
                  ))))

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

#;
(define-syntax (make-spec/name stx)
  (syntax-parse stx
    [(_ name:id)
     #'(begin
         (define all '())
         (define (add spec)
           (set! all (cons spec all))
           )
         (define (get)
           )
         ((list ))
         )]))

(define-for-syntax (make-spec/name)
  (define specs '())
  (define (get) specs)
  (define (add spec) (set! specs (cons spec specs)))
  (values add get))

(define-for-syntax (lookup-name name stx)
    (define (failure) (raise-syntax-error #f "couldn't find any data associated with name" stx))
    (let ([add-get (syntax-local-value name failure)])
      (values (car add-get) (cdr add-get))))

; FIXME if we do this this way then we cannot add new specs at run time ...
; is this ok? yes no maybe?
(define-syntax (get-specs stx)
  (syntax-parse stx
    [(_ name-data:id more-stx)
     (let-values ([(add get) (lookup-name #'name-data #'more-stx)])
       #`(list #,@(get)))
     ])
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
                      .executor
                      .uses
                      .vars
                      .config-vars
                      .inputs
                      .outputs
                      .symret
                      .steps

                      )
    #:local-conventions ([name id]
                         [spec-name id]
                         [specific-name id]
                         [parent-type id]
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
                         [aspect-multi id]
                         [aspect* expr]  ; TODO
                         [oper id]
                         [identifier string]  ; TODO or macro? (DOI: check my syntax please?)
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
     #:with name-data (format-id #'specific-name
                                #:source #'specific-name
                                "~a-data" (syntax-e #'specific-name))
     #:with spec/name (format-id #'specific-name
                                #:source #'specific-name
                                "spec/~a" (syntax-e #'specific-name))
     ; TODO spec/name needs to be a syntax value with a set/get that
     ; collects all the full names for a given name
     #:do ((define-values (add get) (make-spec/name)))
     #:with name-data-stx #`(define-syntax name-data (cons #,add #,get)
                              #;#,(let-values ([(add get) (make-spec/name)]) (cons add get)))
     ;#:do ((define-values (add get) (lookup-name #'name-data stx)))  ; use later
     ;#:with specs #'(let-values ([(add get) (lookup-name #'specific-name stx)]) #`(#,(get))) #;(get-specs (syntax-e #'specific-name))
     #:with specialize-name #`(define (specific-name)
                                "provide the existing information bound to a black-box"
                                `((.type . black-box)
                                  (.name . specific-name)
                                  (.parent . parent-type)
                                  (.docstring . (~? docstring ""))
                                  (.specs ,@(get-specs name-data #,stx))))
     #'(begin
         #;(define-syntax name-stx
           (syntax-property
            (syntax-property
             (syntax-property #'export-stx 'name #'specific-name)
             'spec (list))
            'impl (list)))
         (define name-stx #'export-stx)
         (define name-ast
           '(data export-stx))
         name-data-stx
         specialize-name)
     ]
    [(_ (~or (make name (~optional spec-name)) (:> name (~optional spec-name))) ; make binds the output name as a being/symbol
        ; this approach has the drawback that (make name) is now the only way to refer to this process?
        ; false, that is where impl comes in, but how do we deal with the 1000 different ways to spec
        ; (measure mouse is) ? most of the time we are not going to be 'making' mice... because we
        ; since that is essentially (begin (+ male-mouse female-mouse food water territory) (wait)) -> mice
        (~optional docstring)
        (~optional (.id identifier))
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
     #:with name-data (format-id #'name
                                #:source #'name
                                "~a-data" (syntax-e #'name))
     #|
     #:attr no-bb-yet (if (identifier-binding #'name-data)
                          #f
                          #'(spec (black-box name thing)))
     |#
     #:with spec/name #;(if (attribute spec-name)
                          #;(format-id #'spec-name
                                     #:source #'spec-name
                                     "spec/~a" (syntax-e #'spec-name)))
     (format-id #'(~? spec-name name)  ; FIXME need to support diversity under a single name with identifiers
                #:source #'(~? spec-name name)
                "spec/~a" (syntax-e #'(~? spec-name name)))
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
     #:do ((when (identifier-binding #'name-data)
             (define-values (add get) (lookup-name #'name-data stx))
             (add #'spec/name)
             (void))
           ; TODO on fail create it ...
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
            (if (identifier-binding #'name-data)
                #'(begin
                    ;(~? no-bb-yet)
                    (define spec/name
                      '((.type . make)
                        (.name . (~? spec-name name))
                        ;(.id . (~? identifier))  ; wtf...
                        (.docstring . (~? docstring ""))
                        (.inputs (~? (~@ input ...)) (~? (~@ constrained-input ...)))
                        (.outputs name)
                        (.vars (~? (~@ var ...)))  ; TODO these need to be requested before export to pdf
                        (.steps (~? (~@ step ...)))  ; FIXME if you see ?: attribute contains non-list value it is because you missed ~?
                        (other body ...))))
                #`(begin
                    (spec (black-box name thing))
                    ; recursion here ...
                    #,stx))]
           [old-out
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
    [(_ (~or
         (measure name ...+ aspect)  ; we forth now
         (>^> name ...+ aspect)
         (*: name ...+ aspect)
             )
        ; FIXME why did I allow multiple names here?! Measure the length of all 20 ferrets or something?!
        ; ANSWER we forth now! this is how we support multiple arity aspects
        ; so (has-part? parent child) -> (measure parent child has-part?) same for actualize...
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
                          (.inputs (~? (~@ input ...)) (~? (~@ constrained-input ...)))
                          (.outputs name ...)  ; TODO allow >^ type techniques
                          (.vars (~? (~@ var ...)))  ; TODO these need to be requested before export to pdf
                          ;(.measures (~? (constrained-input aspect* ...)) ...)
                          (.measures aspect (~? (constrained-input aspect* ...)) ...)  ; FIXME aspect black-box binding
                          (.steps (~? (~@ step ...)))  ; FIXME if you see ?: attribute contains non-list value it is because you missed ~?
                          ; the debug message is totally useless :/ took me 3 hours to figure out how to debug it properly
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
    [(_ (inverse-measure name aspect*))
     ;aka actualize for a single aspect... :/ I would prefer to call _this_ actualize
     #''TODO
     ]
    [(_ #;(~or (actualize name) (v> name) (:* name))
        (~or (~or (actualize name [aspect-multi ...])
                  (v> name [aspect-multi ...])
                  (:* name [aspect-multi ...]))
             (~or (actualize name-multi ...+ aspect)
                  (v> name-multi ...+ aspect)
                  (:* name-multi ...+ aspect)))
        ; actualize binds the output name as a process which could have many being outputs
        ; AND MULTIPLE ASPECTS ... HRM TODO
        (~optional docstring)
        ; FIXME aspect!?
        ;(~optional (.uses imports ...))
        (~optional (.vars var ...))
        (~optional (.inputs (~or input [oper constrained-input aspect* ...]) ...))
        (~optional (.outputs outputs ...))
        (~optional (.steps step ...))
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
                          (.inputs (~? (~@ input ...)) (~? (~@ constrained-input ...)))
                          (.outputs name)  ; TODO allow ^> type techniques  FIXME this assumes name:aspect...
                          (.vars (~? (~@ var ...)))
                          (.measures asp??? (~? (constrained-input aspect* ...)) ...)  ; FIXME aspect black-box binding
                          (.steps (~? (~@ step ...)))  ; FIXME if you see ?: attribute contains non-list value it is because you missed ~?
                          (other body ...))
     ; FIXME need a way to check this syntax here and then combine it with impl to form the real output
     ; we do also still want to allow people to view only the spec phase if they want
     (let ([out
            #'(begin
                (define (specification-phase)  ; TODO maybe use parameterization to pass in executors and runtime info?
                  'actualize
                  (~? (define-values (name input ... constrained-input ...)
                        (bind/symbol->being name input ... constrained-input ...))
                      (define (name) (bind/symbol->being name)))
                  #;(~? (define-values (cvar ...) (bind/symbol->being cvar ...)))
                  body ...
                  (define (~? (symbolic-input-phase var ...) (symbolic-input-phase))
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
                         [step sc-step-ref]
                         [type id]
                         [docstring string])
    #:datum-literals (order .executor .vars .inputs .steps)
    #;[(_ name body ...)
       #''TODO]
    #;[(_ (name) body ...)
       #''TODO]
    [(_ (order impl-name)  ; FIXME or is it really (protocol name) and then in the body (order statement ...)
        body ...)
     #'(define impl-name (list body ...))  ; TODO need to deal with if-defined
     ]
    [(_ (~or (spec-name (~optional impl-name))
             (impl-name (~seq #:type type)))  ; FIXME the 2nd option feels like a big ol' mistake...
        (~optional docstring)
        (~optional (.executor executor))
        (~optional (.vars var ...))
        ;(~optional (.inputs input ...))  ; implementation specific inputs i.e. ones that should be results invariant
        (~optional (.inputs (~or input [oper constrained-input aspect* ...]) ...))
        (~optional (.steps step ...))
        ; the right way to implement '.uses' is just detect whether another spec/impl is used in the body
        ; and add it as a dependency in the subprotocols
        body ...)
     #|
     #:with no-spec-yet (if (identifier-binding #'spec/name)
                            #f
                            #'(spec (black-box impl-name thing)))

     |#
     (if (and (attribute spec-name) (identifier-binding (attribute spec-name)))
         ; welp
         ; turns out if you want to use impl directly without a spec either you have to provide a way to declare
         ; type or it just simply will not work becuse we can't really infer the type of thing we are implementing
         ; I guess the crappy way to do this is to allow people to use #:type being #:type aspect #:type ??? here
         ; which is probably ok as a starting point for 1-off protocols, since they are fairly straight forward to
         ; automatically abstract and lift
         #'(begin
             ;(~? no-spec-yet) ; no guts yet
             (define impl/name
               '((.type . make)
                 (.name . (~? impl-name spec-name))
                 ;(.id . (~? identifier))  ; wtf...
                 (.docstring . (~? docstring ""))
                 (.inputs (~? (~@ input ...)) (~? (~@ constrained-input ...)))
                 ; NOTE: in principle every succeeding level should allow the same arguments as the previous level
                 (.outputs (~? impl-name spec-name))
                 (.vars (~? (~@ var ...)))  ; FIXME add the ability to bind these here in impl
                 (.steps (~? (~@ step ...)))  ; FIXME if you see ?: attribute contains non-list value it is because you missed ~?
                 (other body ...))))
         (begin (println (list 'ct: (attribute spec-name) (attribute impl-name)))
                #`(begin (spec ((~? type make) (~? spec-name impl-name)))
                         ; FIXME logic seems unsound here
                         ; FIXME bad way to do defaults should fail instead?
                         #,stx)))
     ]
    )
  )

#;(define-syntax (measure stx)
  (syntax-parse stx
    [(_ thing aspect ...)
     #''TODO])
  )

(module+ test
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
        (.steps "make a thing!")
        "instructions")

  (spec (make thing alt-make-thing)
        ; top level seems to completely klobber these... not even add to them
        (.steps "do a different set of actions and get the same thing!")
        "again ...")

  (spec (actualize pulse [duration electromagnetic])
        (.vars duration-units
               step-units)
        (.inputs [setting pulse-maker
                          ([step-units electromagnetic]
                           [duration-units duration])]
                 ....))

  #;(spec (actualize pulse)
        (.vars duration
               duration-units
               step-size
               step-units)
        (.inputs [setting pulse-maker
                          ([step-units step-size]
                           [duration-units duration])]
                 ....))

  (spec (make loose-patched-cell)
          (.inputs brain-slice
                   internal-solution
                   (: patch-pippette ([MOhms (range 6 10)])))  ; ERROR: don't know how to measure
          (part-of brain-slice cell)
          ;(.output (loose-patch cell patch-pipette))
          )

  (impl (loose-patched-cell)
        (put-a-in-b internal-solution patch-pippette)
        )
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