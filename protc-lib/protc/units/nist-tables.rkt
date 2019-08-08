#lang racket/base
;; https://doi.org/10.6028/NIST.SP.330e2008 The International System of Units
;; Barry N. Taylor, Editor (2006), The International System of Units (SI) (2006 Edition). National Institute of Standards and Technology, Special Publication 330, 2008 Edition. [Online] Available: https://www.nist.gov/pml/special-publication-330 [day, month, year, time]. National Institute of Standards and Technology, Gaithersburg, MD.

;; https://doi.org/10.6028/NIST.SP.811e2008 Guide for the Use of the International System of Units
;; HTML version https://www.nist.gov/pml/special-publication-811
;; see also https://physics.nist.gov/cuu/Units/units.html

;; IVOA recommendation: Units in the VO
;; https://arxiv.org/abs/1509.07267
;; https://arxiv.org/pdf/1509.07267.pdf

(provide si-base-units)

(module get racket/base
  (require
   racket/string
   racket/file
   net/url
   file/cache
   (only-in sxml
            pre-post-order

            sxml:modify
            )
   (only-in sxml/sxpath
            sxpath)
   html-parsing)
  (provide url->port)
  (define default-header
    '())
  (define (url->port url #:header [header default-header])
    (get-pure-port (string->url url) header))

  ;(file->string cache-file)  ; todo ...

  (define (url->port/cached url)
    (cache-file "some"
                #:exists-ok #t
                'some-key
                "/tmp/random-stuff/"
                ""

                )
    )

  (define default (λ (tag . content)
                    (cons tag (filter
                               (λ (t) (or (not (string? t)) (non-empty-string? t)))
                               (content-clean content)))))

  (define detext `((*text* . ,(λ (tag content) (string-trim (string-replace content "\uA0" " "))))
                   (sup . ,(λ (tag content) (string-append "^" content)))
                   (nobr . ,(λ (tag . content) (apply string-append (content-clean content))))
                   (td . ,(λ (tag . content) (apply string-append content)))
                   (th . ,(λ (tag . content) (list tag (if (null? (filter (compose not string?) content))
                                                           (apply string-append content)
                                                           content))))
                   (*default* . ,default)))

  (define dot "·")
  (define (string-clean str)
    (if (string-contains? str dot)
        ; we are definitely a unit
        (string-replace str " " "")
        str))
  (define (content-clean content) (map (λ (t) (if (string? t) (string-clean t) t)) content))
  
  (define (notc predicate)
    (λ (value) (not (predicate value))))

  (define table-clean (apply compose
                             (map sxml:modify
                                  '(("//sup/em/.." delete)
                                    ("//sup[text=\"(e)\"]" delete)
                                    ("//@align" delete)
                                    ("//@style" delete)
                                    ("//@class" delete)
                                    ("//table/text()" delete)
                                    ("//caption/text()" delete)
                                    ("//tr/text()" delete)
                                    ("//br" delete)))))

  (define (process-chapter ch)
    (define tables (cons '*TOP* ((sxpath "//table") ch)))  ; *TOP* required for sxml:modify to work
    (define tables-cleaned (table-clean tables))
    (pre-post-order tables-cleaned detext))

  (define nist-811-ch-4
    "https://www.nist.gov/pml/special-publication-811/nist-guide-si-chapter-4-two-classes-si-units-and-si-prefixes")
  (define ch4 (html->xexp (url->port nist-811-ch-4)))
  (define ch4-out (process-chapter ch4))

  (define nist-811-ch-5
    "https://www.nist.gov/pml/special-publication-811/nist-guide-si-chapter-5-units-outside-si")
  (define ch5 (html->xexp (url->port nist-811-ch-5)))
  (define tables (cons '*TOP* ((sxpath "//table") ch5)))
  (define tables-cleaned (table-clean tables))
  ;(define ch5-out (process-chapter ch5))

  )


; 811-ch-4-table-1
(define si-base-units
  '(
    [name      symbol  base-quantity            ]
    [meter     m       length                   ]
    [kilogram  kg      mass                     ]
    [second    s       time                     ]
    [ampere    A       electric-current         ]
    [kelvin    K       thermodynamic-temperature]
    [mole      mol     amount-of-substance      ]
    [candela   cd      luminous-intensity       ]
    ))

; 811-ch-4-table-2
(define si-units-derived-examples
  '(
    [name                      symbol   derived-quantity]
    [square-meter              m^2      [area]]
    [cubic-meter               m^3      [volume]]
    [meter-per-second          m/s      [speed
                                         velocity]]
    [meter-per-second-squared  m/s^2    [acceleration]]
    [reciprocal-meter          m^-1     [wavenumber]]
    [kilogram-per-cubic-meter  kg/m^3   [density
                                         mass-density]]
    [cubic-meter-per-kilogram  m^3/kg   [specific-volume]]
    [ampere-per-square-meter   A/m^2    [current-density]]
    [ampere-per-meter          A/m      [magnetic-field-strength]]
    [candela-per-square-meter  cd/m^2   [luminance]]
    [mole-per-cubic-meter      mol/m^3  [amount-of-substance-concentration
                                         amount-concentration
                                         concentration]]
    ))

; 811-ch-4-table-3
(define s-units-coherent-derived
  '(
    [name            symbol  simplified   base              derived-quantity]
    [radian          rad     1            m/m               [plane-angle]]
    [steradian       sr      1            m2/m2             [solid-angle]]
    [hertz           Hz      ()           s^-1              [frequency]]
    [newton          N       ()           m·kg·s^-2         [force]]
    [pascal          Pa      N/m2         m^-1·kg·s^-2      [pressure
                                                             stress]]
    [joule           J       N·m          m2·kg·s^-2        [energy
                                                             work
                                                             amount-of-heat]]
    [watt            W       J/s          m2·kg·s^-3        [power
                                                             radiant-flux]]
    [coulomb         C       ()           s·A               [electric-charge
                                                             amount-of-electricity]]
    [volt            V       W/A          m2·kg·s^-3·A^-1   [electric-potential-difference
                                                             electromotive-force]]
    [farad           F       C/V          m^-2·kg^-1·s4·A2  [capacitance]]
    [ohm             Ω       V/A          m2·kg·s^-3·A^-2   [electric-resistance]]
    [siemens         S       A/V          m^-2·kg^-1·s3·A2  [electric-conductance]]
    [weber           Wb      V·s          m2·kg·s^-2·A^-1   [magnetic-flux]]
    [tesla           T       Wb/m2        kg·s^-2·A^-1      [magnetic-flux-density]]
    [henry           H       Wb/A         m2·kg·s^-2·A^-2   [inductance]]
    [degree-celsius  °C      ()           K                 [celsius-temperature]]
    [lumen           lm      cd·sr        Cd                [luminous-flux]]
    [lux             lx      lm/m2        m^-2·cd           [illuminance]]
    [becquerel       Bq      ()           s^-1              [activity-referred-to-a-radionuclide]]
    [gray            Gy      J/kg         m2·s^-2           [absorbed-dose
                                                             specific-energy-imparted
                                                             kerma]]
    [sievert         Sv      J/kg         m2·s^-2           [dose-equivalent
                                                             ambient-dose-equivalent
                                                             directional-dose-equivalent
                                                             personal-dose-equivalent]]
    [katal           kat     ()           s^-1·mol          [catalytic-activity]]
    ))
