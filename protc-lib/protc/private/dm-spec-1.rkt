#lang racket/base
(require ;scribble/srcdoc
         ;(for-doc scribble/base scribble/manual)
         protc/export
         ;protc/utils  ; this is private ...
         rdf/utils
         "direct-model.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/string
                     racket/pretty
                     racket/syntax
                     syntax/parse
                     "direct-model.rkt"
                     "syntax-classes.rkt"
                     "utils.rkt"))

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

(module+ test
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
