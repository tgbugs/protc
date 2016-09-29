;in xrepl use ,en "test.rkt" or (enter! "test.rkt")
#lang racket
(require racket/enter)  ; drracket can't find module->exports w/o this
(require (for-syntax racket/match))

;; useful racket code
; define env
(define (env)
  (map (lambda (x)
	 (let ([y (bytes->string/utf-8 x)])
	   (cons y (getenv y))))
       (environment-variables-names (current-environment-variables))))

; define who

(define rkt-base
  (append
    '(rkt-base)
    (map car (cdar (let-values ([(x y) (module->exports 'racket/enter)]) y)))
    (map car (let-values ([(x y) (module->exports 'racket)])
			 (append (cdaddr x) (cdaddr y))))))

(define (who)
  (let ([all-symbols (namespace-mapped-symbols (current-namespace))])
    (sort (filter (lambda (term) (not (member term rkt-base))) all-symbols) symbol<?)))


;; experimental code

; FIXME something about the way this is defined causes weirdness with match and referring to defrw independently also doesn't give bad syntax errors like it should, needs more work
(define-syntax (defrw stx)  ; FIXME need to support (name arg . args) syntax here
  ; TODO do we want this to handle stuff like checking for defined renamings/synonyms?
  (match (syntax->datum stx)
    [(list _ name-args body)
     ; add code to validate the body
     (displayln (cons "TODO:" body))
     (match name-args
       [(list-rest name rest) (datum->syntax stx `(define ,name-args (list ',name ,@rest)))])]))

;(define-syntax (executor stx))  ; TODO
(define (executor name) "this needs to work like typed racket")

;(define-syntax (executor-namespace stx))  ; TODO
(define (executor-namespace name verbs-nouns) "verbs and nouns should probably be split")

;(define-syntax (def-realworld->digital stx))  ; TODO
(define (def-realworld->digital expression measurement-rule) "measure stuff or other rw->digital transforms")

(defrw (hello thing1 thing2) ("A BODY"))


; we need a variant of defrw that also accepts tools... (written about elsewhere)
(defrw (mix-old thing-to-mix thing-to-mix-with) ("mix thing-to-mix with thing-to-mix-with"))

(mix 'water 'spork) ; -> '(mix water spork) but should '(mix water) this should NOT be hidden
; in the defintion of mix, it completely defeats reusability, and the correct return is actually
; '(mix water) 'spork, but that is a harder issue :/
; ALTERNATELY WHEEE
(define (using noun verb)
  "use noun to verb"
  "verb must be a procedure"
  ; do we want to do anything about attaching/checking if noun can verb?
  ; or somehow annotating verb to show it is a modified verb?
  ; I think it is more important that using constructions account for
  ; NON CONSUMED INPUTS!
  ; but how would/can we use that to build dependency graphs?
  ; I guess we can treat imports of beings as imlicit imports of a 'procure-being'
  ; or 'make-being' function that is not specified here but could be, and it should
  ; be possible to find those functions from the beings using identifiers
   verb)
(defrw (mix thing-to-mix) ("Mix thing. Extend by modifying mix with using."))
; 'mix' is a perfect example of an underspecified function, there is no specification of how long to mix, nor what the criteria would be to determine whether post conditions on mix have been met (contracts?)
; futhermore it raises an important question about whether names as generic as mix are in fact useful outside of use for local quick references
; a 'mixing protocol' could be tagged as such but beyond a VERY cursory writing of a protocol
; it is not useful

; a related issues is the scope of the executor
; if I have a protocol that is about whisking do I say "hold whisk in dominant hand" etc?
; or do I assume they already know the meaning of (whisk thing)?
; the best example being (snibble thing) which is NOT know and can be taught
; OR can be specified using verbs and nouns that are known
(defrw (cut-1 thing) ("NOT ON THE COUNTER TOP YOU IDIOT"))
(defrw (snibble thing) ((using 'knife cut-into-quarter-inch-cubes) thing))
; how to decompose cut-into-quarter-inch-cubes into a verb and a contract is
; a deeper issue

(defrw (cut-on-cutting-board-1 thing) ("Much better"))
(define cut-2 cut-on-cutting-board-1)  ; better for coping with bad names BUT BAD for documenting I/O, a tool is in the name ffs

(defrw (cut-on-cutting-board-2 thing) ((using 'cutting-board 'knife cut-1) thing))  ; BAD
;(defrw (cut thing) (cut thing)) ; ALREADY BAD, TOO ABSTRACT
(defrw (place thing location) ("move the thing to the location"))

(defrw (hold thing-to-hold-with thing-to-hold) ("hold seems simple enough"))
(defrw (position-touching thing-1 thing-2) ("p-t seems simple enough"))
(defrw (move direction thing-to-move) ("move seems simple enough"))

(define human 'human)
(executor human)  ; do it like the type declarations?
(defrw (cut-4 thing implement surface) (
	  (position-touching
	  (hold 'dominant-hand 'implement)
	  (hold 'off-hand (place 'thing 'surface)))
	  (move 'down 'dominant-hand)))  ; aaand we're back to imperative!
	; ARGH order switching of what object actually makes it out of the call! :/
	;(clean surface)
       	;(clean implement)

; this exercise suggest that we really do need to have executor defined namespaces
; where the criteria is that the executor has all the parts specified in the executor namespace
; when an executor is specified the names in that namespace will be recognized within the scope of that definition
(executor-namespace human '(dominant-hand
			     off-hand
			     brain
			     arm
			     index-finger
			     thumb
			     eye
			     teeth))
       

((using 'spork mix) 'water)  ; this is... execution time stuff? but could be put in a closure
((using 'spork mix) '(water salt))  ; FIXME how to allow rest usage?
(define saltwater ((using 'spork mix) '(water salt)))  ; TODO define shorthand for eval time?
;(defrw (add number1 . number2) ("math!"))  ; FIXME example of fail
(defrw (add number1 number2) ("math!"))
((using 'brain add) 1 2)

(define realworld->number (lambda (subset-of-universe) 'number))
(define measure (lambda (subset-of-universe) 'number))

(def-realworld->digital (measure 'being) ('data))  ; 'threading' magic needs to happen here

; a little != implementation why it is not default we will never know
(define (!= a b . rest-id)
  (if (pair? rest-id)
    (not (apply = (cons a (cons b rest-id))))
    (not (= a b))))

; testing out apply-all
(define (apply-all list-of-functions being)
  (foldl (lambda (function result) (function result))
	 being list-of-functions))

(define fun1 (lambda (being) (+ being being)))
(define fun2 (lambda (being) (* being being)))
(define fun3 (lambda (being) (+ being 1)))
(define fun4 (lambda (being) (/ being 2)))

(apply-all (list fun1 fun2 fun3 fun4) 2)

; yogurt example
(define-syntax (repeat-until condition body) ; doesn't quite work
  (define (recurse)
    (if (eval condition)
      #t
      (progn
        (eval body)  ; hidden state, want to get rid of this
        (recurse))))
  (recurse))

(define (has-indentation thing)  ; needs reference points for rigior
  "There is a point on the surface of thing that is
  significantly lower than the surrounding surface"
  (define threshold (units 4 'cm))
  (if (>= (- (get-surface-max thing)
             (get-surface-min thing))
          threshold))

(define (make-indentation-in-a-with-b a b)
  (repeat-until (has-indentation b)
                (press-a-with-b a b))
  (return (bind 'indentation a)))

(define (put-a-in-b a b)  ; need a way to specify executor semantics scope
  "self explanatory given executor semantics"
  ;add a to the contents of b
  (return b))

(define (validate key thing) "if the key exists in the definitions table check it")

(define (make-delicious-yogurt yogurt honey granola blueberries bowl spoon)
  (validate-definition 'yogurt yogurt)
  (return (bind 'delicious-yogurt
    (ordered-sequence-implicit-output-passing ; progn
      (put-a-in-b yogurt bowl)
      (make-indentation-in-a-with-b yogurt spoon)
      (put-a-in-b honey indentation)
      (mix-a-with-b (contents bowl) spoon)
      (put-a-in-b blueberries bowl)
      (mix-carefully-a-with-b (contents bowl) spoon)))))
