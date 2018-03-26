#lang racket/base
(provide units-si)
(define units-si
'((m . meters)
  (g . grams)
  (L . liters)
  (l . liters) ; often seen in ul or ml
  (mol . moles)
  (M . molarity) ; molar
  (_m . molality) ;FIXME molal
  (K . kelvin)
  (°C . degrees-celcius)
  (~oC . degrees-celcius) ; Tom also accepts using the digraph for the degree symbol...
  (ºC . degrees-celcius) ; b'\xc2\xba'
  (◦C . degrees-celcius) ; b'\xe2\x97\xa6' white dot
  (ca . candela)
  (lm . lumens)
  (lx . lux) ; plural?
  (s . seconds)
  (Hz . hertz)
  (min . minutes)
  (h . hours)
  (d . days) ; collides with days?
  (rad . radians)
  (sr . steradians)
  (N . newtons)
  (Pa . pascals)
  (J . joules)
  (W . watts)
  (A . amperes)
  ;(A . amp)
  (C . coulombs)
  (V . volts)
  (F . farads)
  (Ω . ohms) ; b'\xce\xa9'
  (Ω . ohms) ; b'\xe2\x84\xa6'
  (R . ohms) ; R also accepted per the note on wikipedia and brit standard
  (S . siemens)
  (Wb . webers)
  (T . teslas)
  (H . henrys)
  (Bq . becquerels)
  (Gy . grays)
  (Sv . sieverts)
  (kat . katals)
  (dB . decibels))
)