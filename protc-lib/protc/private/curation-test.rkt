#lang racket/base
(require
 racket/pretty
 "identifier-functions.rkt"
 (prefix-in protc: "curation.rkt"))

(protc:aspect "temperature" (hyp: 'asdf))

#;
(protc:arity-binding "how to specify 2-ary systems ..."
                     (protc:black-box-component "interhemispheric sulcus" (hyp: 'lol))
                     (protc:black-box-component "the cutting blade (implied)" (hyp: 'lol))
                     ; alt form (bbc (ab (bbc "a") (bbc "b")) (asp "asp" (param "p")))
                     ; leads to question of how to specify the arity of an aspect

                     ; the actual way to tag this is probably
                     ; (bbc "a" (bbc "b" (asp "c")) possibly with implied-bbc

                     ; what physical beings form the two lines?
                     ; angle is not an aspect of a single thing
                     ; it is a 2-ary aspect of course not all pairs
                     ; of things have an angle aspect and actually
                     ; * having an existential crisis * WHAT IS AN ANGLE?!
                     ; angles are like distances, they just require
                     ; a third point, so technically this is a 3-ary
                     ; but in many cases the point can be specified as ∀
                     ; points where thing a intersects thing b the angle
                     ; shall be ... for more fun, in 3d you also need to
                     ; specify the projection plane of the angle
                     (protc:aspect "angle" (hyp: 'another-annotation)))

(protc:black-box-component "interhemispheric sulcus" (hyp: 'lol)
                           (protc:black-box-component "the cutting blade (implied)" (hyp: 'lol))
                           (protc:aspect "angle" (hyp: 'another-annotation)))

(protc:black-box-component "interhemispheric sulcus" (hyp: 'lol)
                           (protc:black-box-component "the cutting blade (implied)" (hyp: 'lol)
                                                      (protc:aspect "angle" (hyp: 'another-annotation))))

(protc:aspect "count" (hyp: 'max)
              (protc:parameter* (param:quantity 99) (hyp: 'take-one-down))
              (protc:parameter* (param:quantity 98) (hyp: 'pass-it-around)))

(protc:output "something with a pH" (hyp: 'a)
              (protc:parameter* (param:quantity 7.4 (param:prefix-unit 'pH)) (hyp: 'QaiHMm5kEee7iSNIQtkMfg))
              )

(protc:output "anger" (hyp: '0)
              (protc:aspect "level" (hyp: '1) (protc:parameter* (quantity 10 (unit 'joules 'giga)) (hyp: '2)))
              (protc:input "some additional input" (hyp: 'none))
              )
#;
(pretty-print spec/_0)

(protc:output "solution" (hyp: 'tybs4nDBEee42j9uY1eDGg)  ; https://hyp.is/tybs4nDBEee42j9uY1eDGg
  (protc:aspect "temperature" (hyp: 'vqTANHDBEeejPh8if_gYww)  ; https://hyp.is/vqTANHDBEeejPh8if_gYww
    (protc:parameter* (protc:fuzzy-quantity "room temperature" "temperature")
                       (hyp: 'YGqyVmzWEeeH6BvIRfBZ-g)))  ; https://hyp.is/YGqyVmzWEeeH6BvIRfBZ-g
  (protc:input "mixture" (hyp: 'rTIL9HDBEeeX4ZvrqxuNcw)  ; https://hyp.is/rTIL9HDBEeeX4ZvrqxuNcw
    (protc:input "methanol" (hyp: 'npCJQHDBEeeCEBvix6mE3w)  ; https://hyp.is/npCJQHDBEeeCEBvix6mE3w
      (protc:invariant (param:quantity 25 (param:unit 'percent)) (hyp: 'JGQ2iGzWEee4-WcT0ahWww)))  ; https://hyp.is/JGQ2iGzWEee4-WcT0ahWww
    (protc:input "water" (hyp: 'o5pFNHDBEeeCr6vQt2P8bA)  ; https://hyp.is/o5pFNHDBEeeCr6vQt2P8bA
      (protc:invariant (param:quantity 75 (param:unit 'percent)) (hyp: 'KHgmqGzWEeebQn8cupmYfw))))  ; https://hyp.is/KHgmqGzWEeebQn8cupmYfw
  (protc:input "uranylacetate" (hyp: 'mkHDrnDBEeejPRcwocKQ-Q)  ; https://hyp.is/mkHDrnDBEeejPRcwocKQ-Q
    (protc:invariant (param:quantity 4 (param:unit 'percent)) (hyp: 'IOx4OmzWEee7AqdCPNmOrw))))

#;
(pretty-print spec/tybs4nDBEee42j9uY1eDGg)

; FIXME NOTE had to change this from input ... 
(protc:output "bright-fieldmicroscope" (hyp: 'PuCBiHFZEeeTpQM8U0RHoQ)  ; https://hyp.is/PuCBiHFZEeeTpQM8U0RHoQ
  (protc:input "objective" (hyp: 'ObFbEHFZEeen1j-7rNCHog)  ; https://hyp.is/ObFbEHFZEeen1j-7rNCHog
    (protc:implied-aspect "immersion type" (hyp: 'HOcIHnFYEeeNAGNA-1qqyw)  ; https://hyp.is/FEwKknFYEeeiQe9ERBKndQ
      (protc:parameter* (protc:fuzzy-quantity "water" "immersion-type")
                         (hyp: 'FEwKknFYEeeiQe9ERBKndQ)))  ; https://hyp.is/FEwKknFYEeeiQe9ERBKndQ
    (protc:implied-aspect "immersion type" (hyp: 'elb2qHFYEeeRZEc4fG4mPg)  ; https://hyp.is/bh6EWnFYEeeFBVeiZ_mndQ
      (protc:parameter* (protc:fuzzy-quantity "water" "immersion-type")
                         (hyp: 'bh6EWnFYEeeFBVeiZ_mndQ)))  ; https://hyp.is/bh6EWnFYEeeFBVeiZ_mndQ
    (protc:implied-aspect "immersion type" (hyp: '2wQEWnFYEeeHYg-CtbOJSQ)  ; https://hyp.is/zq7H0HFYEeehc3-ZJk9KVQ
      (protc:parameter* (protc:fuzzy-quantity "water" "immersion-type")
                         (hyp: 'zq7H0HFYEeehc3-ZJk9KVQ)))  ; https://hyp.is/zq7H0HFYEeehc3-ZJk9KVQ
    (protc:implied-aspect "magnification" (hyp: '9KsiwnFXEeek7R8N1RoJNQ)  ; https://hyp.is/7bGZ2HFXEeejoTcpUOcDDA
      (protc:parameter* (param:quantity 40 (param:unit 'fold)) (hyp: '7bGZ2HFXEeejoTcpUOcDDA)))  ; https://hyp.is/7bGZ2HFXEeejoTcpUOcDDA
    (protc:implied-aspect "magnification" (hyp: 'UA03DnFYEeepLkNB-AmkhA)  ; https://hyp.is/SZbdCHFYEeeHYeP1yePN5A
      (protc:parameter* (param:quantity 60 (param:unit 'fold)) (hyp: 'SZbdCHFYEeeHYeP1yePN5A)))  ; https://hyp.is/SZbdCHFYEeeHYeP1yePN5A
    (protc:implied-aspect "magnification" (hyp: 't3fwbnFYEee5QZuQbSvUUg)  ; https://hyp.is/sVr-7HFYEeepL4sKO3m8Zw
      (protc:parameter* (param:quantity 100 (param:unit 'fold)) (hyp: 'sVr-7HFYEeepL4sKO3m8Zw)))  ; https://hyp.is/sVr-7HFYEeepL4sKO3m8Zw
    (protc:implied-aspect "numerical aperture" (hyp: 'YzoOQnFYEeejojsqDaPT_Q)  ; https://hyp.is/VORBoHFYEeeDHi91q1s02g
      (protc:parameter* (param:quantity 0.9) (hyp: 'VORBoHFYEeeDHi91q1s02g)))  ; https://hyp.is/VORBoHFYEeeDHi91q1s02g
    (protc:implied-aspect "numerical aperture" (hyp: 'dwXDVnHXEeeoAU9-Y_zpSg)  ; https://hyp.is/-m66tnFXEeehcpNRRtxeCw
      (protc:parameter* (param:quantity 0.75) (hyp: '-m66tnFXEeehcpNRRtxeCw)))  ; https://hyp.is/-m66tnFXEeehcpNRRtxeCw
    (protc:implied-aspect "numerical aperture" (hyp: 'w5BUzHFYEeeq_edigYmC8w)  ; https://hyp.is/vbIP3HFYEeeBJ_syZ8AgXw
      (protc:parameter* (param:quantity 1.3) (hyp: 'vbIP3HFYEeeBJ_syZ8AgXw)))))

#;
(pretty-print spec/PuCBiHFZEeeTpQM8U0RHoQ)

(protc:output "ACSF" (hyp: 'fRo-0m5kEeedKdsE58_hHg)  ; https://hyp.is/fRo-0m5kEeedKdsE58_hHg
  (protc:input "CO2" (hyp: 'v1ItZG5kEeedKtcodoX5mg)  ; https://hyp.is/v1ItZG5kEeedKtcodoX5mg
    (protc:invariant (param:quantity 5 (param:unit 'percent)) (hyp: 's69awm5kEeejWzem-0PTmQ)))  ; https://hyp.is/s69awm5kEeejWzem-0PTmQ
  (protc:input "CaCl2" (hyp: 'm752UG5kEeeaDsdQ4-JFGQ)  ; https://hyp.is/m752UG5kEeeaDsdQ4-JFGQ
    (protc:invariant (param:quantity 2 (param:unit 'molarity 'milli)) (hyp: 'L7w9hG5kEeemP4-3u5OMBQ)))  ; https://hyp.is/L7w9hG5kEeemP4-3u5OMBQ
  (protc:input "KCl" (hyp: 'juGpZm5kEeeDRKNvzurMkA)  ; https://hyp.is/juGpZm5kEeeDRKNvzurMkA
    (protc:invariant (param:quantity 2.5 (param:unit 'molarity 'milli)) (hyp: 'KPipam5kEeep3XfwWZ5EvQ)))  ; https://hyp.is/KPipam5kEeep3XfwWZ5EvQ
  (protc:input "MgSO4" (hyp: 'oql7QG5kEee2X8-GimceZQ)  ; https://hyp.is/oql7QG5kEee2X8-GimceZQ
    (protc:invariant (param:quantity 2 (param:unit 'molarity 'milli)) (hyp: 'NUJ_Fm5kEeeQunNPx36ERA)))  ; https://hyp.is/NUJ_Fm5kEeeQunNPx36ERA
  (protc:input "NaCl" (hyp: 'hZajtm5kEee7istrvqec3Q)  ; https://hyp.is/hZajtm5kEee7istrvqec3Q
    (protc:invariant (param:quantity 124 (param:unit 'molarity 'milli)) (hyp: 'IfxKpG5kEeeMXSMnam2aGA)))  ; https://hyp.is/IfxKpG5kEeeMXSMnam2aGA
  (protc:input "NaH2PO4" (hyp: 'lbQ2PG5kEeesibtfidVCBA)  ; https://hyp.is/lbQ2PG5kEeesibtfidVCBA
    (protc:invariant (param:quantity 1.25 (param:unit 'molarity 'milli)) (hyp: 'LIQSNm5kEeesacscRusrXg)))  ; https://hyp.is/LIQSNm5kEeesacscRusrXg
  (protc:input "NaHCO3" (hyp: 'iasYLm5kEee4ovMH8nDy9A)  ; https://hyp.is/iasYLm5kEee4ovMH8nDy9A
    (protc:invariant (param:quantity 26 (param:unit 'molarity 'milli)) (hyp: 'JPdzbm5kEeeVxx-pUxw6RQ)))  ; https://hyp.is/JPdzbm5kEeeVxx-pUxw6RQ
  (protc:input "O2" (hyp: 'ubUzOG5kEeeyK-_pttNP4A)  ; https://hyp.is/ubUzOG5kEeeyK-_pttNP4A
    (protc:invariant (param:quantity 95 (param:unit 'percent)) (hyp: 'sJARLm5kEeeBAPf8ofR3UQ)))  ; https://hyp.is/sJARLm5kEeeBAPf8ofR3UQ
  (protc:input "glucose" (hyp: 'qo2d8G5kEeeaVtfbBH7_Tw)  ; https://hyp.is/qo2d8G5kEeeaVtfbBH7_Tw
    (protc:invariant (param:quantity 10 (param:unit 'molarity 'milli)) (hyp: 'OwIthG5kEeePCIdTAQ5bFQ)))  ; https://hyp.is/OwIthG5kEeePCIdTAQ5bFQ
  (protc:parameter* (param:quantity 7.4 (param:prefix-unit 'pH)) (hyp: 'QaiHMm5kEee7iSNIQtkMfg)))

(pretty-print spec/fRo-0m5kEeedKdsE58_hHg)
