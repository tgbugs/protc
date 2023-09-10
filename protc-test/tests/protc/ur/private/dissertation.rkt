#lang racket/base
; the except-in is not needed at the repl, but is required in module context
(require (except-in protc/ur #%top) protc/private/curation protc/private/curation-unprefixed)

(output "DIO mouse"
  (aspect "type identifier" (invariant "JAX:380050"))
  (aspect "mass" (parameter* (quantity 52 (unit 'grams))))
  (input "mouse"
    (aspect "age" (parameter* (quantity 6 (unit 'weeks)))))
  (input "mouse diet"
    (aspect "ad libitum" (parameter* (bool #t)))
    (aspect "type identifier" (invariant "D12392"))))

(executor-verb "Anesthetize"
  (input "isoflurane"
    (implied-aspect "percent volume"
      (invariant (quantity (expr (range 1 3)) (unit 'percent)))))
  (input "mouse")
  (input "nose cone"))

(executor-verb "Place"
  (input "heating pad")
  (input "mouse" (aspect "supine")))

(executor-verb "Make"
  (black-box-component "cervical region")
  (black-box-component "incision"
    (implied-aspect "length"
      (parameter* (quantity (expr (range 1 1.5))
                            (unit 'meters 'centi)))))
  (implied-aspect "length"
    (parameter* (quantity (expr (range 2 3)) (unit 'meters 'milli))))
  (implied-aspect "location"
    (parse-failure #:node-type 'invariant
                   #:failed-input "lateral to midline"))
  (implied-aspect "location"
    (parse-failure #:node-type 'invariant #:failed-input "left side")))

(executor-verb "separate"
  (black-box-component "carotid artery")
  (black-box-component "left cervical vagus nerve"))

(executor-verb "Clamp"
  (black-box-component "mouse paw")
  (input-instance "MouseOx sensor")
  (objective* "for vitals measurement"))

(executor-verb "Affix"
  (input "GRIN lens"
    (implied-aspect "dimensions"
      (parameter*
       (dimensions (quantity 1 (unit 'meters 'milli))
                   (quantity 9 (unit 'meters 'milli))))))
  (input "cuff")
  (input "super glue"))

#; ; should error
(protc:aspect "count"
  (protc:input "mouse"
    (protc:parameter* (param:quantity 10))))

(protc:input "mouse"
  (protc:aspect "count"
   (protc:parameter* (param:quantity 10))))
