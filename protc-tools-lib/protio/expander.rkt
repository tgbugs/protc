#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))
(provide
 (rename-out [protio-module-begin #%module-begin])
 (except-out (all-from-out racket/base) #%module-begin)
 (all-defined-out))

(define-syntax (protio-module-begin stx)
  (syntax-parse stx
    [(_ ast)
     #'(#%module-begin
        (require racket/pretty)
        (provide result)
        ast
        ;(pretty-print result)
        )]))

(define result '())

(define (doc . expressions)
  (set! result expressions))

(define (expression value)
  value)

(define (io input output)
  (case (list input output)
    ['("?" "?") 'hasPrimaryParticipant]
    ['("?" ">") 'hasPrimaryOutput]
    ['(">" "?") 'hasPrimaryInput]
    ['(">" ">") 'hasPrimaryInputOutput]
    ;['("?" ">'") 'modifiesPrimaryOutput]  ; this is exactly equivalent to mPIO the ? is not allowed
    ['(">" ">'") 'modifiesPrimaryInputOutput]
    [else (raise-syntax-error 'u-wot-m8 (format "this should never happen ~a ~a" input output))]))

(define (triple input aspect-value output)
  (list (io input output) (aspect aspect-value)))

(define (mod input aspect-value output-prime)
  (list (io input output-prime) (aspect aspect-value)))

(define (maybe-input value)
  (if (equal? value "?")
      ;'hasPrimaryParticipantMaybeInputNoOutput
      'hasPrimaryParticipantUnbinding
      ;'hasPrimaryInputNoOutput
      'hasPrimaryInputUnbinding))

(define (maybe-output value)
  (if (equal? value "?")
      'hasPrimaryParticipantNoInput  ; this is the one we don't understand
      'hasPrimaryOutputNoInput))

(define (pair input-aspect aspect-output)
    (if (member input-aspect '("?" ">"))  ; >' does not apply here
        (list (maybe-input input-aspect)
              ; it's an input
              (aspect aspect-output))
        (list (aspect input-aspect)
              ; its an aspect
              (maybe-output aspect-output))))

(define (aspect value)
  (cond [(or (equal? value "|")
             (equal? value ":")  ; this is the best since we already use it for aspects anyway! :)
             (equal? value "."))  ; NOTE eq? ONLY _SOMETIMES_ succeeds here
         'hasPrimaryAspect] ; nothing?
        [(equal? value "^") 'hasPrimaryAspectMeasured]
        [(equal? value "v") 'hasPrimaryAspectActualized]
        [else (raise-syntax-error 'u-wot-m9 (format "This should never happen ~s" value))]))

(define (single value)
  (list 'hasPrimaryParticipantNoInputNoOutput
        (aspect value)))
