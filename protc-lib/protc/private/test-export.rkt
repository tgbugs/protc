#lang racket/base
(require "direct-model.rkt")
; FIXME big old mess here with wrong types for everything and underspecified missing higher level definitions etc.
(spec (measure cell spike?)
      "measure whether ~a spikes"
      ;(.uses)
      ;(.inputs cell)
      ;(.outputs)
      ;(.vars)
      ;(.measures)
      (.steps
       "in current clamp mode"
       "watch the voltage trace"
       "if there is a spike there will be a small deflection"))

(spec (measure cell EPSP?)
      "measure whether ~a produces an EPSP"
      ;(.uses)  ; cell membrane potential voltage-trace
      ;(.inputs cell)
      ;(.outputs)
      ;(.vars)
      ;(.measures)
      (.steps
       "in current clamp mode"
       "watch the voltage trace"
       "there will be an inward deflection")
      )

(spec (measure cell-a cell-b projects-a-b?)  ; FIXME these should probably be impl??
      "measure whether ~a projects to ~a"  ; debug messages are so bad here, this is why we don't use strings kids
      ;(.uses)
      ;(.inputs cell-a cell-b)
      ;(.outputs)
      ;(.vars)
      ;(.measures)
      (.steps
       (spike? cell-a)
       (EPSP? cell-b)
       ; FIXME allow (and (spike? cell-a) (EPSP? cell-b))
       "(define projects-a-b (and (spike? cell-a) (EPSP? cell-b))"  ; FIXME actual
       ))

(spec (measure cell loose-patch)  ; FIXME
      "create a low resistance seal between ~a and ~a"
      ;(.uses)
      ;(.inputs pipette cell)
      (.inputs pipette)
      ;(.outputs)
      ;(.vars)
      ;(.measures)
      (.steps
       "use the controller to move the pipette tip to touch the cell"
       "once they are touching wait"
       ))

(spec (measure cell whole-cell-patch)  ; FIXME obviously wrong type
      "create a high resistance seal between ~a and the interior of ~a"
      ;(.uses)
      ;(.inputs pipette cell)
      (.inputs pipette)
      ;(.outputs)
      ;(.vars)
      ;(.measures)
      (.steps
       "use the controller to move the pipette tip to touch the cell"
       "once they are touching wait"
       "apply a slight negative pressure until a gia ohm seal forms"
       ))

(spec (measure cell-a cell-b test-connected-pair)  ; FIXME again obviously wrong type, this should just be a technique
      "test whether ~a is connected to ~a using two of ~a"
      ;(.uses)
      (.inputs #;brain-slice
               ;cell-a
               ;cell-b
               patch-pipette  ; FIXME technicalyy need more than one :/
               )
      ;(.outputs)
      (.vars command-potential
             pipette-resistance
             pulse-current
             pulse-duration)
      (.measures spike? EPSP? projects-a-b?)  ; FIXME xml escape! and auto lifting!
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
  (list spec/*test-connected-pair))
(provide protc-for-export)  ; TODO compiled protc modules should export this automatically
(define spec/*test-connected-pair-ast spec/*test-connected-pair)
(export spec/*test-connected-pair 'html #;'pdf)
