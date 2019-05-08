#lang racket/base
(provide units-dimensionless units-dimensionless-prefix)
(define units-dimensionless
'((% . percent) ; mass fraction vs mole fraction
  (NA . numerical-aperture)
  ;(x . fold)
  ;(× . fold) ; b'\xc3\x97'
  )
)
(define units-dimensionless-prefix
'((pH . pH)
  (x . fold)
  (× . fold) ; b'\xc3\x97'
  )
)
