#lang racket/base
(require (rename-in racket/base [string char->string]))
(require parsack)
(require "../../../ni/protocols/rkt/units/si-prefixes-data.rkt")
(require "../../../ni/protocols/rkt/units/si-prefixes-exp-data.rkt")
(require "../../../ni/protocols/rkt/units/si-units-data.rkt")
(require "../../../ni/protocols/rkt/units/si-units-extras.rkt")
(require "../../../ni/protocols/rkt/test-params.rkt")

(define (include-all-cdrs alist)
  (append alist
          (for/list ([pair alist]) (cons (cdr pair) (cdr pair)))))

(define (apply-pair function pair)
  (cons (function (car pair)) (function (cdr pair))))

(define (lookup-from-alists alist . alists)
  (define pairs (include-all-cdrs (append alist (apply append alists))))
  (for/list ([pair pairs]) (cons (symbol->string (car pair)) (cdr pair))))

(define (len-sort a b) (> (string-length (car a)) (string-length (car b))))  ; longest first

(define (make-with-parser-generator-return parser alist)
  (define sorted (sort alist len-sort))
  (for/list ([pair sorted])
    (let ((match (car pair))
          (return-value (cdr pair)))
      (>> (parser match) (return return-value)))))
  ;(for/list ([pair sorted]) (list '>> (list 'string (car pair)) (list 'return (cdr pair)))))

(define (make-with-string-return alist)
  (make-with-parser-generator-return string alist))

(define (make-with-any-case-return alist)
  (make-with-parser-generator-return oneOfStringsAnyCase alist))

(define (return-string->number string)
  (return (string->number string)))

(define (param symbol)
  (define namespace:symbol (string->symbol (string-append "param:" (symbol->string symbol))))
  (define (put-before return-value)
    (if return-value
        (if (list? return-value)
            (return (cons namespace:symbol return-value))
            (return (list namespace:symbol return-value)))
        (return '())))
  (lambda (parser)
    (>>= parser put-before)))

(define (try-choice . rest)
  (apply <or> (map try rest)))

(define (AT-MOST-ONE parser)
  (<any> parser (return '())))

(define unit-lookup (lookup-from-alists units-si units-extra))
(define prefix-lookup (lookup-from-alists prefixes-si))
(define DEGREES-UNDERLINE (bytes->string/utf-8 (bytes #xc2 #xba)))
(define DEGREES-FEAR (bytes->string/utf-8 (bytes #xe2 #x97 #xa6)))
(define EN-DASH (bytes->string/utf-8 (bytes #xe2 #x80 #x93)))
(define HYPHEN-MINUS (bytes->string/utf-8 (bytes #x2d)))

(define spaces (many $space))
(define carrot (string "^"))
(define exponent (>> carrot (return 'exponent))) ; how to modify output?
(define division (string "/"))
(define multiplication (string "*"))
(define unit-op (<or> division multiplication))
(define colon (string ":"))
(define plus-or-minus-symbol (string "±"))
(define plus-or-minus-pair (string "+-"))
(define plus-or-minus (>> (<or> plus-or-minus-symbol plus-or-minus-pair)
                          (return 'plus-or-minus)))
(define en-dash (string EN-DASH))
(define hyphen-minus (string HYPHEN-MINUS))
(define dash-thing (>> (<or> en-dash hyphen-minus) (return HYPHEN-MINUS)))
(define double-dash-thing (parser-compose dash-thing dash-thing))
(define thing-accepted-as-a-dash (>> (<or> double-dash-thing dash-thing) (return HYPHEN-MINUS)))
(define gt (string ">"))
(define gte (string ">="))
(define lt (string "<"))
(define lte (string "<="))
(define comparison (<or> gt gte lt lte))
(define CROSS (bytes->string/utf-8 (bytes #xc3 #x97)))
(define cross (string CROSS))
(define x (string "x"))
(define by (<or> x cross))
(define approx (>> (string "~") (return '(approximately))))
(define number-word-lookup
  '(("zero" . 0)
    ("one" . 1)
    ("two" . 2)
    ("three" . 3)
    ("four" . 4)
    ("five" . 5)
    ("six" . 6)
    ("seven" . 7)
    ("eight" . 8)
    ("nine" . 9)))

(define (return-char->string chars)
  ;(println chars)
  (return (apply char->string chars)))
(define num-word (apply try-choice (make-with-any-case-return number-word-lookup)))
(define digits (many $digit))
(define digits1 (many1 $digit))
(define point (string "."))
(define int-string (>>= (parser-compose (AT-MOST-ONE dash-thing) digits1)
                        return-char->string))
(define int (>>= int-string return-string->number))
(define (join . rest)
  ;(println rest)
  (apply append rest))
(define float-string (>>= (parser-seq (AT-MOST-ONE dash-thing)
                                      (<or> (parser-seq digits1 point digits #:combine-with join)
                                            (parser-seq digits point digits1 #:combine-with join)) #:combine-with append)
                          return-char->string))
(define float (>>= float-string return-string->number))
(define E (string "E"))
(define scientific-notation (>>= (parser-compose (<or> float-string int-string) E int-string) return-string->number))
(define num (try-choice scientific-notation
                        float
                        int
                        num-word))
(define prefix-symbols (apply try-choice (make-with-string-return prefix-lookup)))
(define unit-symbols (apply try-choice (make-with-string-return unit-lookup)))
(define unit-atom (<or> (parser-seq prefix-symbols
                                    unit-symbols
                                    #:combine-with (lambda (a b) (cons b a)))
                        unit-symbols))
(define unit-base (parser-seq
                   unit-atom
                   (>> spaces
                       (AT-MOST-ONE
                        (parser-seq
                         (AT-MOST-ONE exponent)
                         (>> spaces int))))))
(define unit (parser-seq  ; consider parser-compose
              unit-base
              (>> spaces (AT-MOST-ONE unit-op))
              (many (>> spaces unit))
              #:combine-with join))
(define to (string "to"))
(define range-indicator (>> (<or> thing-accepted-as-a-dash to) (return 'range)))
(define pH (string "pH"))
(define P (string "P"))
(define postnatal-day (>> P (return 'postnatal-day)))
(define fold-prefix (>> (parser-seq by (lookAhead num)) (return 'fold)))  ; don't use endBy here it is a many0 in parsack
(define prefix-unit-base (<or> pH postnatal-day fold-prefix))
(define prefix-unit ((param 'prefix-unit) prefix-unit-base))
(define prefix-quantity (parser-seq
                         prefix-unit
                         (>> spaces num) #:combine-with (lambda (pu n) (list n pu))))
(define percent (string "%"))
(define fold-suffix (>> (parser-seq by (lookAhead (<or> $eof (<!> num)))) (return 'fold)))  ; FIXME
(define suffix-unit-no-space ((param 'unit) fold-suffix))
(define suffix-unit ((param 'unit)
                     (<or> percent unit)))
(define suffix-quantity (parser-seq num
                                    (<any> suffix-unit-no-space
                                           (>> spaces (AT-MOST-ONE suffix-unit)))))
(define quantity ((param 'quantity) (<or> prefix-quantity suffix-quantity)))
(define dilution-factor ((param 'dilution) (parser-seq int (~ colon) int)))
;(define dimensions ((param 'dimensions) (sepBy1 quantity (>> spaces (parser-seq by (lookAhead quantity))))))  ; not quite... since it will match 1x ...
;(define (skip p q) (>>= p (lambda (return-value) (>> q (return return-value)))))  ; maybe useful?
(define dimensions ((param 'dimensions) (parser-seq quantity
                                                    (~ (>> spaces by))
                                                    (>> spaces quantity)
                                                    (many (>> (>> spaces by) (>> spaces quantity)))
                                                    ;#:combine-with 
                                                    )))
(define prefix-operator (<or> plus-or-minus comparison))
(define infix-operator (<or> plus-or-minus range-indicator multiplication division exponent))
(define prefix-expression (parser-seq prefix-operator (>> spaces quantity)))
(define infix-expression (parser-seq
                          quantity
                          (>> spaces infix-operator)
                          (<or> infix-expression quantity)))
(define expression ((param 'expression) (<or> prefix-expression infix-expression)))  ; TODO prefixed infix...
(define C-for-temp ((param 'unit) (>> (string "C") (return (cons 'degrees-celcius '())))))
(define temp-for-biology (parser-seq num C-for-temp))
(define parameter-expression (parser-seq
                              (AT-MOST-ONE approx)
                              (>> spaces (try-choice ; indeed needed instead of <or>
                                          dimensions
                                          dilution-factor
                                          temp-for-biology
                                          expression
                                          quantity
                                          (return 'FAILURE)))
                              #:combine-with append))

(define (test)
  (define tests (list "1 daL" "300 mOsm" "0.5 mM" "7 mM" "0.1 Hz." "-50 pA"
                      "200–500mm" "0.3%–0.5%" "1:500" "4%" "10 U/ml"
                      "–20°C" "<10 mV" "–70 ± 1 mV" "30 to 150 pA"
                      "310 mosmol/l" "13–16 days old" "50 x 50 um"
                      "~3.5 - 6 Mohms" "pH 7.3" "17–23 d old" "10 –100"
                      "250 +- 70 um" "20±11 mm" "+- 20 degrees"
                      "0.1 mg kg–1" "75  mg / kg" "40x" "x100"
                      "200μm×200μm×200μm" "20--29 days" "4 °C" "10×10×10"
                      "10 kg * mm^2 / s^2" "10 * 1.1 ^ 30 / 12"))

  (println (parse ((param 'butts)
                   (>>= (string "butts") return-char->string)) 
                  "butts"))
  (println (parse exponent "^10"))
  (println (parse plus-or-minus "+-"))
  (println (parse plus-or-minus "±"))
  (println (parse thing-accepted-as-a-dash "--"))

  ; units
  ;(println (sort (lookup-from-alists units-si units-extra) len-sort))
  ;(println (sort (lookup-from-alists prefixes-si) len-sort))
  (println (parse prefix-symbols "yotta"))
  (println (parse prefix-symbols "milli"))
  (println (parse prefix-symbols "mil"))
  (println (parse prefix-symbols "m"))
  (println (parse prefix-symbols "d"))  ; FIXME needs sorting
  (println (parse prefix-symbols "da"))
  (println (parse prefix-symbols "dB"))
  (println (parse unit-symbols "m"))
  (println (parse unit-symbols "L"))  ; FIXME HOLY WAT
  (println (parse unit-atom "daL"))
  (println (parse unit-base "daL"))
  ;(println (parse unit "111111"))
  (println (parse unit "daL"))
  (println 'U-WOT-M8)
  ;(println (parse suffix-unit "11111"))
  (println (parse suffix-unit "daL"))

  ;(println (parse prefix-unit "111111"))  ; WAT (fixed)
  ;(println (parse prefix-unit "p"))
  ;(println (parse prefix-unit ""))
  ;(println (parse prefix-unit "daL"))
  (println (parse prefix-unit "P"))
  ;(println (parse prefix-unit "x"))
  (println (parse prefix-unit "x10"))
  (println (parse prefix-unit "pH"))
  (println (parse prefix-quantity "pH 7.5"))
  (println (parse prefix-quantity "x10"))
  (println (parse prefix-quantity "P16"))

  ; numbers
  (println (parse int "1 daL"))
  (println (parse float "1000.0999 daL"))
  (println (parse float "1.0 daL"))
  (println (parse num "1"))
  (println (parse num "1 "))
  (println (parse num "1 daL"))
  (println (parse quantity "1 daL"))
  (println (parse prefix-expression "<10 mV"))

  ; wow bugs
  (println (parse quantity "200μm×200μm×200μm"))
  ;(println (parse prefix-expression "200μm×200μm×200μm"))
  ;(println (parse infix-expression "200μm×200μm×200μm"))
  (println (parse parameter-expression "200μm×200μm×200μm"))
  (println (parse dimensions "200μm×200μm"))
  (println (parse dimensions "200μm×200μm×200μm"))
  (println (parse dimensions "200μm×200μm×200μm×200μm"))
  ;(for/list ([t tests]) (begin (print t) (println (parse parameter-expression t))))
  (for/list ([t param-test-strings]) (begin (print t) (println (parse parameter-expression t))))
  ;(time (for/list ([t param-test-strings]) (parse parameter-expression t)))
  'nop
  )

(test)
