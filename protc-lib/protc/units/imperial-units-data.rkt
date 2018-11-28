#lang racket/base
(provide units-imp)
(define units-imp
'((ft . feet)
  (slugs . slugs)
  (°F . degrees-fahrenheit)
  (~oF . degrees-fahrenheit) ; Tom also accepts using the digraph for the degree symbol...
  (ºF . degrees-fahrenheit) ; b'\xc2\xba'
  (◦F . degrees-fahrenheit) ; b'\xe2\x97\xa6' white dot
  (lbs . pounds))
)
