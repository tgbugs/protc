#lang racket/base
(provide units-dimensionless units-dimensionless-prefix)
(define units-dimensionless
'((% . percent) ; mass fraction vs mole fraction
  (count . count) ; when something has been counted and the unit type comes from the parent (sort of a dimensionless quantity but not quite...)
  (times . count)
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
