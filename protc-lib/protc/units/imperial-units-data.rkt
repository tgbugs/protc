#lang racket/base
(provide units-imp)
(define units-imp
'((ft . feet)
  (in . inches)
  (slugs . slugs)
  (°F . degrees-fahrenheit)
  (~oF . degrees-fahrenheit) ; Tom also accepts using the digraph for the degree symbol...
  (ºF . degrees-fahrenheit) ; b'\xc2\xba'
  (◦F . degrees-fahrenheit) ; b'\xe2\x97\xa6' white dot
  (lb . pounds)  ; FIXME normalization
  (lbs . pounds)
  (RCF . relative-centrifugal-force)
  (Fr . french-gauge)  ; also Fg and F but F has many collisions with degrees-fahrenheit
  (G . birmingham-gauge))
)
