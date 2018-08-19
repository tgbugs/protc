#lang racket/base
(require protc/private/direct-model)

(spec (black-box thing thing)
      "Any nameable subset of physical reality existing at a single point in time")

(spec (measure parent child has-part?)  ; vs (measure has-part?) (.inputs child)
      (.symret boolean?))

#;
(impl (has-part?)
        (.executor human)
        (and (observe? parent)  ; this is mostly implicit observe? -> (true? (*observe parent))
             (observe? child)
             ; oh look, just follow the pattern if you don't know what to do!
             ; actualize* takes a thing and an aspect which means that you need both!
             ; and that way the thing you are actualizing/measuring will always be present
             ; for symbolic manipulation and examination!
             ; implicit when? or how to communicate this...

             ; this is perfect, order and order* used this way allow use to
             ; explictily create the scope in which a measurement is going to
             ; be made and do all the symbolic things that we want with them
             ; afterward in the body, the instructions will translate to
             ; "displace the parent in x, y, z space and record how much you moved it"
             (do* [(:* parent displacement vector3)
                      (*: child displacement vector3)]
                  (= (: parent displacement)
                     (: child displacement)))

             ; more redable version
             ; note that sticking the vecotr type on the end is adding an explicit data type
             ; which is impl... the spec could have just displacement and then we need a way
             ; to allow the implementation to specify that that is displacement in
             ; (param:dimensions (param:unit 'meters)
             ;                   (param:unit 'meters)
             ;                   (param:unit 'meters))
             ; (dim 'meter 'meter 'meter) (dim-n 3 'meter) vector3 'meter somehow...
             (do* [(actualize parent displacement vector3)
                   (measure child displacement vector3)]
                  (= (: parent displacement)
                     (: child displacement)))
             #;
             (do*
              ; actually I think having the scope helps a whole lot
              ; because it says that the values are those that are in
              ; the context of _this_ action, there could be many actions
              (:* parent displacement vector3)
              (*: child displacement vector3)
              (= (: parent displacement) (: child displacement)))
             ; basically begin
             #; ; old version, which sort of works but is probably overly verbose
             (do* ([action (:* parent displacement vector3)]
                   ; we could define move* as (:* parent displacement vector3) ...
                   ; and that would allow us to propagate the fact that when you move something
                   ; there is some displacement, whether you actually measured it or not will only
                   ; matter if you do something with it down the line
                   [outcome (*: child displacement vector3)])
              #;([action (move* parent vector3)]  ; vs move:distance*
                   [outcome (*move:vector3 child)])

                  (= (: action vector3) (: outcome vector3))
                  )))

(spec (measure a b different?)
      "Thing a and thing b are not the same thing."
      "They can be separated from eachother or move independently of eachother."
      "Neither has-part? the other as defined in direct model"
      ; see around L570
      (assert (not (and (has-part? a b)
                        (has-part? b a)))))

(spec (measure thing composite?)
      ; composite-under-naming?
      ; hasCompositeNamingRule
      "A thing whose type name is the same as the name given to subsets of itself.
For example any subset of a pile of salt is still salt.
Contrast this with a human being, where one subset is called and arm and another a leg."
      (define-being s1 (being-subset thing))
      (define-being s2 (being-subset thing))
      (for-all (s1 s2)
               (given ((being->type-name thing)
                       (different? s1 s2))
                      (same (being->type-name thing)
                            (being->type-name s1)
                            (being->type-name s2)))))


(spec (actualize thing allocation)
      "Divide ~a into two subsets, one matching the size of ~a."
      ; FIXME this is annoying ... because there is number here ...
      ; and actualize is for a number I need generic processes that
      ; actually create black box subsets not just make the whole black box

      ; in a strict sense, subsetting in the real world is actually taking a named
      ; or unnamed subset of a named black box and separating it from the black-box
      ; the thing is that that action has a name but is only guranteed to produce
      ; a name that matches (action thing), and is not required to have a name itself
      ; this is essentially what verbs are, and we need them ...

      ; In THEORY we could try to wedge this into actualize, but the question is what
      ; aspect would we choose? the amount aspect? nope, allocation (yep yep yep)

      ; the subset is the thing, the allocation is the aspect
      ; allocate or subset is the verb, we should add the ability to provide a verb name...

      ; this also vastly improves communication because now it is clear that I need to tell
      ; you some ammount to allocate, even if its approximate
      ;#:verb allocate  ; subset  ; HRM not clear we want this
      ; since it reads fine to actualize the allocation of a thing
      ; as part of series of steps
      (.expects (composite? thing))
      ;(.outputs subset-of-thing (- thing subset-of-thing))  ; primary output goes first
      ; since subset itself is undefined but sexps provide a better way to name things
      (.outputs '(subset thing) (- thing '(subset thing))))

(spec (measure thing mass)
      )


(impl (actualize thing mass)
      (.expects
       (organism? thing)
       ;(grows? thing)
       )
      (until
       (>= (mass thing) mass)
       ;(>= (measure thing mass) mass)  ; lol maximum confusion
       ; setting spec-value here is essentially forcing (.vars mass-value)
       ; to be implicitly registered at spec time, or rather that we will
       ; have to raise an error if and actual value is not bound by an impl
       ; that inherits from this one, ie specifically a case where the
       ; ((actualize mass) mouse 50g) needs to occur in a body ...

       ; hrm, one way around this is to just make it clear that measure
       ; always binds references to aspects as aspects, and use of variables
       ; bound to aspects outside measure (and possibly some other cases)
       ; will result in the passed in value being used?
       ; hrm ...

       ; if we want the aspect be available for doing logic
       ; and also at runtime we have to have a way to denote that...
       ; (>= (measure thing (run-time mass)) (spec-time mass))

       "Keep the calories in higher than the calories out"
       "or if it is a crystal keep adding mass/feedstock etc"
       "Keep adding mass ..."
       ))

(impl (actualize thing [allocation mass])
      ; FIXME this form of impl has to have a name because thing is already taken
      ; thing-allocation-mass might work, or maybe evn just allocation-mass
      ; or just to confuse everyone completely why not (allocation mass)?! (derp)
      ; actually ... this can just pattern match
      ; (actualize salt (allocation salt) 50g) should have
      ; ((measure mass) (subset salt)) -> 50g
      ; the only question is how to differentiate subsets

      ;(.expects (composite? thing))  ; this is compile time and should not be exported
      ; another way to deal with generics/types that either the user might need to supply

      ; we don't actually need expects, because the inclusion of +ammount+ allocation as an aspect
      ; makes it much clearer what is going on

      ; fun: you could automatically detect ethical violations using a function like this ...
      ; (actualize )

      ;(define-aspect (value-much-less-than some mass))
      ;(define-aspect some mass)
      ;(define some (λ (value) (< value mass)))
      (define some (make-value))
      (invariant (< some mass))  ; perferably much less than
      ; also automatically type inference
      ; make sure this usage scopes correctly...
      ; this would be super confusing because mass here would
      (put-a-on-b (allocation thing some)))

(impl (actualize solution volume)
      (.inputs solvent)
      (define dv/dt (make-value
                     "The volume added per unit time should be small enough that if the
                      executor has bad reaction time they can still stop pouring in time."))
      ; formalized this would be
      (invariant solvent (< dv/dt (/ (expt (- final-volume (*: solution volume)) 1.1)
                                     ; 1.1 gives something like an 80/20 rule
                                     (* (lookup executor reaction-time) 10))))
      (until (= (*: solution volume) volume)
             (add-a-to-b solvent solution)))

(impl (actualize concentration)
      (do-practical* ([(actualize mass) solute]
                      [put-a-in-b solute solvent]))
      )

(impl (conc-by-known-mass)
      )

(spec (actualize solute concentration)
      (.vars final-volume)
      (.inputs solvent)  ; can this be used to handle implicit inputs?
      ; (.expects solvent) might be a clean way to do this
      ; if something with name solvent exists in the calling context of this function
      ; do we need to require people to list it? what kinds of trouble will we create
      ; by 'smart' (aka probably stupid) binding of variables in the calling context
      (= concentration (/ (/ (measure solute mass)
                             ; this actualize will have to be called at some point
                             ; in an impl, but it doesn't have to be _this_ impl
                             (actualize solvent final-volume))
                          (lookup solute (/ mass (numerator concentration)))))
      ; rosette will create the program that can invert this at runtime
      ; when final volume is supplied
      )

