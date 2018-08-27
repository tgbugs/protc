#lang racket/base

(require racket/string (for-syntax racket/base syntax/parse "utils.rkt"))
(provide (all-defined-out))

; TODO sync with ontology

(define-syntax (define-aspect stx)
  ; FIXME this is still not as useful as I would like ...
  (syntax-parse stx
    [(_ shortname:id name:id
        (~optional (~seq #:parent parent))
        constructive-definition:expr  ; FIXME need a way to look inside of these
        )
     #;
     ; TODO need this this as a way to document the arity of our aspects, also makes more sense logically
     ; HOWEVER given that my use of aspect is _very_ close to 'measureable' this may be moot ...
     ; and multi arity aspects will simply be treated as 'complex' and requiring some computation
     ; e.g. (kD/allolosteric enzyme modulator) where temperature is implicit
     ; as opposed to chained or typed aspects, where act mass is different from act [allocation mass]
     (_ (~or (aspect:id being-name-multi:id ...+)
             ([aspect-multi:id ...] being-name:id))
        (~optional (~seq #:parent parent:id))
        (~optional (~seq #:abbrev shortname:id))  ; aka shortaspect
        constructive-definition:expr  ; FIXME need a way to look inside of these
        )
     ;#:with aspect/name (fmtid "aspect/~a" #'(~? aspect (aspect-multi ...)))  ; FIXME need (: a b c) macro
     #:with aspect/name (fmtid "aspect/~a" #'name)
     #:attr aspect/shortname (if (and (attribute shortname)
                                      (not (eqv? (syntax-e #'name)
                                                 (syntax-e #'shortname))))
                                 (fmtid "aspect/~a" #'shortname)
                                 #f)
     #:attr -shortname (if (eqv? (syntax-e #'shortname) (syntax-e #'name)) #f #'shortname)
     #:attr aspect/parent (if (attribute parent) (fmtid "aspect/~a" #'parent) #f)
     #:with parent-data (if (attribute parent) (fmtid "~a-data" #'parent) #f)  ; this canont use attr??? or maybe it can?
     #:attr parent-add (if (attribute parent) (fmtid "~a-add" #'parent) #f)
     #:attr aspect/parent-stx (if (attribute aspect/parent)
                                  ; FIXME get the original location of the syntax
                                  #; ;FIXME why does this break!?
                                  (syntax-local-value #'aspect/parent)
                                  #'(parent #:children #f)
                                  #f)
     #:with aspect-data (fmtid "~a-data" #'name)
     #:with aspect-add (fmtid "~a-add" #'name)
     #:with aspect-get (fmtid "~a-get" #'name)
     #:attr shortaspect-data (if (attribute aspect/shortname) (fmtid "~a-data" #'shortname) #f)
     #:with shortaspect-add (fmtid "~a-add" #'shortname)  ; ok to define this unconditionally
     #:with shortaspect-get (fmtid "~a-get" #'shortname)  ; ok to define this unconditionally
     #:attr shortaspect-add-stx (if (attribute aspect/shortname)
                                    #'(define-syntax (shortaspect-add stx)
                                        (syntax-parse stx
                                          [(_ value)
                                           #'(aspect-add value)]))
                                    #f)
     #:attr shortaspect-get-stx (if (attribute aspect/shortname)
                                    #'(define-syntax (shortaspect-get stx)
                                        aspect-data)
                                    #f)

     #:with childs #'(.children ,@(map (λ (proc) (if (procedure? proc)
                                                     (begin (println proc)
                                                            ; here's the infinite loop we've been waiting for
                                                            (proc #:parent #f))
                                                     proc))
                                       (aspect-get)
                                       ))
     #:with data-alist #``((.name . name)
                           (.shortname . shortname)
                           (.def . constructive-definition)
                           (.parent (~? ,@aspect/parent-stx))
                           childs
                           )
     #:attr aspect/name-stx #'(define aspect/name data-alist) ; TODO rosette integration
     #:attr aspect/shortname-stx (if (attribute aspect/shortname)
                                     #'(define aspect/shortname aspect/name)
                                     #f)
     #:attr aspect-parent-add-stx (if (attribute parent-add)
                                      (begin
                                        #;  ; debug
                                        (println `(adding ,(syntax->datum #'aspect/name) to
                                                          ,(syntax->datum #'parent)) )
                                        #'(parent-add 'aspect/name))
                                      #f)
     ; TODO syntax-local-value to look this stuff up for use in rosette
     #`(begin
         ;(provide aspect/name (~? aspect/shortname))
         aspect/name-stx
         (~? aspect/shortname-stx)  ; have to use this form, otherwise it seems that the nested missing parent will force skip all...
         (define-for-syntax aspect-data '())
         (~? (define-for-syntax shortaspect-data aspect-data))
         (define-syntax (aspect-add stxi)
           (syntax-parse stxi
             [(_ value)
              #;  ; debug
              (println `(adding ,(syntax->datum #'value) to name))
              #'(begin-for-syntax
                  (set! aspect-data (cons value aspect-data))
                  (~? (set! shortaspect-data aspect-data))
                  )]))
         (define-syntax (aspect-get stx)
           #`(quote #,aspect-data))

         (~? shortaspect-add-stx)
         (~? shortaspect-get-stx)

         (~? aspect-parent-add-stx)
         (define (name #:children [c #t] #:parent [p #t] [data data-alist])
           ; FIXME can we do this at compile time?
           (println (list 'rt: name c p))
           (if c
               (if p
                   data
                   (let ([r (reverse data)])
                     (reverse (cons (car r) (cddr r)))))
               (let ([r (reverse data)])
                 (if p
                     (reverse (cdr r))
                     (reverse (cddr r)))))
           #;
           (cons `(.children ,@(get-specs aspect-data #,stx))
                 data-alist))
         (~? (define (-shortname #:children [c #t] #:parent [p #t]) (name #:children c #:parent p)))
         )
     ]
    ))

; http://www.ibiblio.org/units/ also of interest perhaps

(define simple-aspects
  ; technically this is one -> many but we take the default here
  #hash((meters . length)
        ; going with length as more fundamental than distance because
        ; length is context free, whereas distance requires start and end
        (grams . mass)
        (liters . volume)
        (moles . amount)
        (molarity . concentration)  ; [:: molar concentration]  moles concentration? molar is the aspect?
        (molality . molality) ; mols per mass SOLVENT _big_ difference, some people say not to call this concentration ...
        (kelvin . temperature) ; thermodynamic temperature
        (degrees-celcius . temperature)  ; relative temperature
        (candela . [:: luminous intensity])  ; flux / solid angle
        (lumens . [:: luminous flux])  ; [:: radiant flux] is more fundamental and is measured in watts
        (lux . [:: luminous flux density])  ; illuminance (officially) lm/m**2 ala teslas = webers/m**2, radiant flux density, or irradiance flux density >_< is indeed how this unit is referred to
        (seconds . time)
        (minutes . time)
        (hours . time)
        (days . time)
        (hertz . frequency)
        (radians . angle) ; plane angle
        (steradians . solid-angle) ; volume angle
        (newtons . force)
        (pascals . pressure)
        (joules . energy)
        (watts . power)
        (amperes . [:: electrical current])
        (coulombs . [:: electrical charge])
        (volts . [:: electrical potential]) ;
        (farads . [:: electrical capacitance])
        (ohms . [:: electrical impedance])  ; impedance is more fundamental than resistance
        (siemens . [:: electrical conductance])
        (henrys . [:: electrical inductance])  ; magnetic flux / amp
        (webers . [:: magnetic flux])
        (teslas . [:: magnetic flux density])  ; HRM can we (magnetic flux density) this? magnetic field strength
        (becquerels . [:: radioactive decay frequency])  ; 'specific activity' decay vs event ... statsitical, decay vs decaying?
        (grays . [:: radioactive ionizing dose absorbed])
        (sieverts . [:: radioactive ionizing dose equivalent])
        (katals . [:: catalytic amount frequency])  ; mols/s catalysis? amount / second catalytic activity
        (decibels . [:: logarithmic ratio]) ; technically not si? used as a prefix, but not a normal si prefix signal-to-noise ratio
        (pH . acidity)  ; TODO
        (percent . proportion)  ; TODO naming on this one ...
        ))

(define (unit->aspect unit)
  (let [(aspect (hash-ref simple-aspects unit 'unknown))]
    (if (list? aspect)
        (string->symbol  ; FIXME possibly redundant
         (string-join (map symbol->string (cdr aspect)) "-"))
        aspect)))

(module+ test
  (define-aspect sasp some-aspect "some aspect parent")
  ;(debug-repl)
  ;(println (list "its workgin!??!" (get-specs sasp-data #'lol)))
  (sasp)
  ; LOL wow, ok, well, if you call (sasp) here, and a child has been defined
  ; then it will try to reference aspect/some-aspect-child and fail ... wat
  (define-aspect saspc some-aspect-child #:parent sasp "some aspect child")
  (sasp)
  (saspc)
  (unit->aspect 'meters)
  )
