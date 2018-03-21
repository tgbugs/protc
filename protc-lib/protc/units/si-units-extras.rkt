#lang racket/base
(provide units-extra units-extra-prefix) ; hoho
(define units-extra
'((Osm . osmoles) ; note: not all of these are actually si units...
  (eV . electron-volts) ; argh spaces
  (U . enzyme-units) ; measure based on amount of enzyme activity
  (° . degrees) ; of angle/arc hopefully
  (d . days)
  (w . weeks)
  (months . months)
  (Y . years) ; lowercase y causes day -> deca years FIXME maybe we need units-extra-not-prefixable?
  ;(osmol . osmoles) ; shows up as osmol/l which suggests that Osm maybe osmolarity since it leaves out the /l?
  (count . count) ; when something has been counted and the unit type comes from the parent (sort of a dimensionless quantity but not quite...)
  )
)
(define units-extra-prefix
'((P . postnatal-day)
  )
)
