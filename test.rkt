(define-syntax-rule (defrw name args body) `(define ,name (,@args) (list ',name ,@args)))

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
    (map car (let-values ([(x y) (module->exports 'racket)])(append (cdaddr x) (cdaddr y))))))

(define (who)
  (let ([all-symbols (namespace-mapped-symbols (current-namespace))])
    (sort (filter (lambda (term) (not (member term rkt-base))) all-symbols) symbol<?)))
