;Sun Jun 24 15:06:47 2018
#lang racket/base
(require racket/pretty)

(require scribble/base
         scribble/core
         scribble/decode
         scribble/render
         scribble/base-render
         scribble/html-properties
         scribble/latex-properties
         scribble/latex-prefix
         scribble/xref
         racket/dict
         racket/list
         racket/class
         racket/format
         racket/string
         racket/system
         racket/path
         racket/runtime-path
         barcode/qrcode
         ;(only-in scribble/html/html form)
         (prefix-in xml/ xml)
         (prefix-in html/ scribble/html)
         (only-in scribble/manual manual-doc-style)
         (prefix-in pdf: scribble/pdf-render)
         (prefix-in latex: scribble/latex-render)
         (prefix-in html: scribble/html-render)
         (for-syntax racket/base syntax/parse racket/syntax)
         )


(provide protc->scribble
         scribble->many
         scribble->html
         scribble->tex
         scribble->pdf
         export
         export-file
         get-user
         (struct-out user))

(struct user (name orcid)
  #:inspector (make-inspector))

#;(define cstyle (html-defaults
                  "scribble-prefix.html"
                  "manual-style.css"
                  (list "manual-files.css"
                        )
                  ))
#;(css-style-addition "manual-racket.css")



#;(mapping
   (spec
    (-> spec part)
    (-> name title)  ; top level only? how to embed subspecs
    (-> inputs block tabular)
    (-> outputs block tabular)
    (-> symbolic-inputs block tabular)
    (-> symbolic-outputs block tabular)  ; aka measures
    (-> subprotocol part)
    )
   )

(define (zip-blank . lists)
  (let* ([lens (map length lists)]
         [maxl (apply max lens)]
         [adds (map (λ (l) (make-list (- maxl l) null)) lens)])
    ;(println (~a lens lists adds))
    (apply map list (map append lists adds))  ; this works because apply constructs (map list rest1 rest2 ...)
    ))

(define (zip-check . lists)
  "checkboxes!")
(define (current-timezone)
  (date*-time-zone-name (seconds->date (current-seconds))))

(define-runtime-path protc-style-css "protc-style.css")
(define-runtime-path protc-style-tex "protc-style.tex")
(define-runtime-path protc-prefix-tex "protc-prefix.tex")

; extracted from (decode (list (title "hello world")))
(define protc-style (style
                     #f
                     (list
                      (document-date "")
                      (document-version "")   ; TODO versioning for individual protocols?
                      ;(color-property "black")
                      ;(color-property "red")
                      (latex-defaults
                       ;'(collects #"scribble" #"manual-prefix.tex")
                       protc-prefix-tex ; TODO rename?
                       '(collects #"scribble" #"manual-style.tex")
                       '())
                      (tex-addition protc-style-tex)  ; TODO path wrangling
                      (html-defaults
                       '(collects #"scribble" #"scribble-prefix.html")
                       '(collects #"scribble" #"manual-style.css")
                       '((collects #"scribble" #"manual-fonts.css")))
                      (css-style-addition '(collects #"scribble" #"manual-racket.css"))
                      (css-style-addition protc-style-css)
                      (js-style-addition '(collects #"scribble" #"manual-racket.js")))))

(define style-table-fit
  (style #f (list (attributes '((type . "fit")))))
  )


(define box "\u2610")  ; these kills pdflatex, have to \DeclareUnicodeCharacter
(define check-box "\u2611")
(define non-breaking-hyphen "\u2011")  ; WHY w3c WHY
(define word-joiner "\u2060")
(define zero-width-nbsp "\ufeff")

(define (simple-part #:style [style-current protc-style] . blocks)
  (let ([tag-prefix "protc"]
        [tags '()]
        [title-content '()]
        ;[style-current manual-doc-style]
        [to-collect '()]
        ;[blocks ]
        [parts '()] )
    (part tag-prefix
          tags
          title-content
          style-current
          to-collect
          blocks
          parts))
  )

(define (sub-protocol title steps #:style [style-current protc-style])
  "When exporting for execution, any tree structure must be flattened.
   If there is a defined order, then that flattening will follow that order.
   If there is NOT a defined order, then flattening will occur in the order
   in which sub protocols are referenced in the body of a spec."
  (let ([tag-prefix "protc"]
        [tags '()]
        [title-content (list title)]
        ;[style-current (style #f '(unnumbered))]
        ;[style-current manual-doc-style]
        [to-collect '()]
        ;[blocks (list (paragraph manual-doc-style str))]
        [parts '()])
    (part tag-prefix
          tags
          title-content
          style-current
          to-collect
          (list (apply itemlist #:style 'ordered
                       (map (λ (step) (item step)) steps)))
          parts))
  )

(define (scribble->html parts-list #:name [file-name "test-protc-output"])
  (render parts-list
          (list (format "~a.html" file-name))
          #:dest-dir "html"
          #:render-mixin html:render-mixin))

(define (scribble->tex parts-list #:name [file-name "test-protc-output"])
  (render parts-list
          (list (format "~a.tex" file-name))
          #:dest-dir "html"
          #:render-mixin latex:render-mixin))

(define (scribble->pdf parts-list
                       #:name [file-name "test-protc-output"]
                       #:xelatex [xelatex #f]
                       )
  (render parts-list
          (list (format "~a.pdf" file-name))
          #:dest-dir "html"
          #:render-mixin (if xelatex
                             pdf:xelatex-render-mixin  ; FIXME broken?
                             pdf:render-mixin)
          ))

(define lut 
  (hash 'html scribble->html
        'tex scribble->tex
        'pdf scribble->pdf))

(define (scribble->many parts-list #:name [file-name "test-protc-output"] . formats)
  
  (map (λ (fmt) ((hash-ref lut fmt) parts-list #:name file-name)) formats)
  (void))

(define (make-form measure-name) ; appears on every page
  (let ([sa         string-append]
        [emptylabel "...enter data..."]
        [dimcolor   "#888"])
    (html/form '([class "dataform"])  ; TODO protocol name
               (html/input
                '([class (~a "datafield-" measure-name)]
                  [style ,(sa "color: "dimcolor";")]
                  [type "text"]
                  [value ,emptylabel]
                  [title "Enter a search string to search the manuals"]
                  [onkeypress ,(format "return DoDataEnter(event, this, ~s, ~s);"
                                       (version) measure-name)]
                  [onfocus ,(sa "this.style.color=\"black\"; "
                                "this.style.textAlign=\"left\"; "
                                "if (this.value == \""emptylabel"\") this.value=\"\";")]
                  [onblur ,(sa "if (this.value.match(/^ *$/)) {"
                               " this.style.color=\""dimcolor"\";"
                               " this.style.textAlign=\"center\";"
                               " this.value=\""emptylabel"\"; }")])))))

(define (intersperse seperator xs)
  (cond
    [(null? xs) '()]
    [(null? (cdr xs)) xs]
    [else (cons (car xs)
                (cons seperator
                      (intersperse seperator (cdr xs))))]))

(define (form #:class [class "werk werk!"]
              ;#:action [action "HRM"]  ; not needed, just send it to its own page? or do we need a way to redirect?
              . content)
  (elem #:style (style #f (list 
                           protc-style
                           (xexpr-property
                            (xml/cdata #f #f (format "<div>\n<form class=~s method=~s target=~s>"
                                                     class "post" "_self"))
                            (xml/cdata #f #f "</form>\n</div>"))))
        ;(intersperse (linebreak) content)
        content
        ))

(define (input #:type [type "text"]
               #:id [id #f]
               #:name [name #f]
               #:placeholder [placeholder #f]
               #:value [value #f]
               #:p [p #t]
               #:label [label #f]
               #:form [form #f]
               . content)
  (elem #:style (style #f (list 
                           (xexpr-property
                            (let ([type (format " type=~s" type)]
                                  [tag-id (if id (format " id=~s" id) "")]
                                  [name (if name (format " name=~s" name) "")]
                                  [placeholder (if placeholder (format " placeholder=~s" placeholder) "")]
                                  [value (if value (format " value=~s" value) "")]
                                  [label (if label (format "<label for=~s>~a</label>" id label) "")]
                                  [checkmark (if (equal? type "checkbox")
                                                 "<span class=\"checkmark\"></span>"
                                                 "")]
                                  [form (if form (format " form=~s" form) "")]
                                  )
                              ; the comment here allows toggleing between the html representation and the latex repr
                              (xml/cdata #f #f (format "~a~a<input~a~a~a~a~a~a>~a<!--"
                                                       (if p "<p>\n" "") label type tag-id name placeholder value form checkmark)))
                            (xml/cdata #f #f (if p "-->\n</p>" "-->")))))
        (if label 
            (cons label content)
            content)
        ))

(define (button #:type [type "submit"] . content)
  (elem #:style (style #f (list 
                           (xexpr-property
                            (xml/cdata #f #f (format "<button type=\"~a\">" type))
                            (xml/cdata #f #f "</button>"))))
        content
        ))

(define (div #:class [class #f] . content)
  (elem #:style (style #f (list 
                           (xexpr-property
                            (let ([class (if class (format " class=~s" class) "")])
                              (xml/cdata #f #f (format "<div~a>" class)))
                            (xml/cdata #f #f "</div>"))))
        content
        ))

(define (colorize #:color c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

(define (red . content)
  (colorize #:color "red" content))

(define (WARNING #:message [message "Something went wrong!"] . content)
  (list
   (colorize #:color "red" content)
   ; margin-note* fails inside of tabular and causes latex errors
   (margin-note message)))

(define (cbox input-name #:form [form "inputs"])
  (div #:class "checkbox"
       (input #:type "checkbox"
              #:id input-name
              #:name input-name
              #:p #f
              ;#:label ""
              #:form form box)))
(define texin "__________")

(define (get-units measure-name)
  ; TODO actual lookup
  ; noting the fact that this lookup table is going
  ; to have to keep track of how aspects are bound to beings
  ; because (: mouse mass) and (: anesthetic mass) have different units
  ; and there may be more than one anesthetic with the same units too
  (case measure-name
    [(anesthetic-dose) 'mg/kg]
    [(concentration) 'mM]
    [(slice-thickness) 'um]
    [(time-to-cell-death) 'mins]
    [(number-of-cells) 'count]
    [(command-potential) "~-60 mV"]
    [(pipette-resistance) "6-10 MΩ"]
    [(pulse-current) "~.1-1 nA"]
    [(pulse-duration) '5ms]
    [(spike?) 'boolean]  ; TODO checkbox
    [(EPSP?) 'boolean]  ; TODO checkbox
    [(projects-a-b?) 'boolean]  ; TODO checkbox
    [else 'unit]))

(define (check-units [add-warning (λ (warn) (void))])
  (define (inner-check-units measure-name)
    (let* ([units (get-units measure-name)]
           [sunits (format "~a" units)]  ; FIXME compound units...
           [sname (symbol->string measure-name)]
           )
      (case units
        [(mg/kg) (begin (add-warning (list (red "mg/kg:") " Don't know how to bind two masses")) (red sname))]
        [(count) (begin (add-warning (list (red "count:") " I don't know how to count that...")) (red sname))]
        [else sname]
        ))
    )
  inner-check-units)

(define (cinput measure-name #:form [form "results"])
  (input #:type "text"
         #:id measure-name
         #:name measure-name
         #:placeholder (format "~a" (get-units measure-name))  ; unfortunately can't WARN here...
         #:form form texin))

(define (make-margin notes-list)
  (margin-note 
   (apply itemlist #:style 'ordered
          (map (λ (step) (item step)) notes-list)))
  (margin-note (intersperse (linebreak) notes-list))
  )

(define (protc->scribble ast #:user [export-user null])
  ; TODO target is probably needed here? inversion of control or if statement?
  ; https://docs.racket-lang.org/scribble/core.html#(part._.Structure_.Reference)
  ; https://docs.racket-lang.org/scribble/pict_2.png map protc to this model
  (pretty-print ast)
  (define dalist (cadr ast))
  (define (dr key) (dict-ref dalist key))
  (define name (dr '.name))
  (define docstring (dr '.docstring))
  (define subprotocols (dr '.subprotocols))
  ;(define entries (map (λ (row) (map ~a row)) (dr '.inputs)))
  (define inputs (cons '("Inputs") (map (compose list ~a) (dr '.inputs))))
  (define outputs (cons '("Outputs") (map (compose list ~a) (dr '.outputs))))
  (define steps (dr '.steps))
  (define unit-warnings '())
  (define (add-warning warning)
    (set! unit-warnings (cons warning unit-warnings)))
  (define table-cols
    (list (cons (list check-box) (map (λ (i) (cbox i)) (dr '.inputs)))
          inputs
          outputs
          (cons '("Variables") (map (compose list (check-units add-warning)) (dr '.vars)))  ; TODO these need to be resolved
          ; the best way to do this is probably to provide an export function that wraps
          ; protc->scribble which accepts a let-like ([var 1] [var2 1001]) spec and then prompts for missing
          ; that can be written in plain racket in the file and will be disregarded when it comes to the actual protocol
          (cons '("Values") (map (λ (v) (cinput v)) (dr '.vars)))
          (cons '("Measures") (map (compose list (check-units add-warning)) (dr '.measures)))
          ;(cons '("Results") (map (compose list html/form) (car (dr '.measures))))  ; TODO html looks like it needs to be specialized
          (cons '("Results") (map (λ (m) (cinput m)) (dr '.measures)))
          )
    )
  (println (zip-blank inputs outputs))
  ;(println inputs)
  (define sub-style (style #f '(unnumbered)))
  (define style-current protc-style)
  (define maybe-user-qr
    (if (null? export-user) ""
        (let ([filename "/tmp/user-qr.png"])
          (make-qrcode (user-orcid export-user) 'L #:filename filename)
          (image filename #:scale 0.25 #;#:style #;'center))))

  (define (flatten-subs subs)
    (pretty-print subs)
    (let* ([dalist subs]  ; note the absense of the outer data here since we use name-stx
           [dr (λ (key) (dict-ref dalist key))]
           [name (dr '.name)]
           [x (println name)]  ; wat
           [title (symbol->string name)]
           [steps (dr '.steps)]
           [args (list title steps)]
           [subs-next (dr '.subprotocols)])
      ; simple ordering rule, impl will introduce additional ordering rules
      (if (null? subs-next)
          args
          (reverse (cons args (reverse (map flatten-subs subs-next)))))))
  (define sub-protocols 
    (let ([fs (map flatten-subs subprotocols)])
      ;(pretty-print fs)
      ;(pretty-print (apply append fs))
      (map (λ (sp) (apply sub-protocol sp)) (apply append fs)))
    
    #;(list 
       (sub-protocol "spike?" '("in current clamp mode"
                                "watch the voltage trace"
                                "if there is a spike there will be a small deflection"))
       (sub-protocol "EPSP?" '("in current clamp mode"
                               "watch the voltage trace"
                               "there will be an inward deflection"))
       (sub-protocol "projects-a-b?" '("(define projects-a-b (and (spike? cell-a) (EPSP? cell-b))"))))
  
  (define scrib 
    (let ([tag-prefix "protc"]
          [tags '()]
          [title-content (~a "Specification of " name)]  ; TODO qr -> data deposit
          ;[style-current (style #f '(unnumbered))]
          #;[style-current null
                           
                           ]
          [to-collect '()]  ; TODO subprotocols?
          #;[blocks (list
                     (paragraph (style #f '(unnumbered))
                                (~a "Execution date ____-__-__\n T __:__ " (current-timezone)))
                     (tabular (apply zip-blank table-cols) #:sep (hspace 2)
                              #:row-properties '(bottom-border ())
                              ;#:style 'boxed
                              )
                     #;(tabular inputs #:sep (hspace 2)
                                #:row-properties '(bottom-border )
                                ;#:style 'boxed
                                )
                     #;(tabular outputs
                                #:sep (hspace 2)
                                #:row-properties '(bottom-border )
                                ;#:style 'boxed
                                )
                     )]
          [blocks (let ([base (list
                               (para (~a "Executor: " (if (null? export-user)
                                                          "____________________"
                                                          (~a (user-name export-user)
                                                              " "
                                                              (user-orcid export-user))))
                                     ;(image "/home/tom/pictures/ZjL1bb3.jpg") ; no step on senk
                                     maybe-user-qr
                                     )
                               (para (~a "Execution date ____-__-__\n T __:__ " (current-timezone)))
                               ; TODO execution date fills automatically in html...
                               (tabular (apply zip-blank table-cols) #:sep (hspace 2)
                                        #:row-properties '(bottom-border ())
                                        #:style style-table-fit)
                               ;(para (html/xml->string (html/form "work" (html/input "data"))))
                               ; (render-element (html/form "work" (html/input "data")))
                               ;(paragraph style-current (html/form "work" (html/input "data")))
                               

                               ; spec time known values
                               ; compile/export time known values
                               ; run time values that have to be collected but are not really measures, e.g. mouse number
                               ; run time and compile time need to be able to exchange places fluidly depending on
                               ; exactly what the user can supply at compile time
                               )])

                    (if (null? unit-warnings)
                        base
                        (cons (make-margin (reverse unit-warnings)) base)))
                  
                  ]
          [parts (cons (sub-protocol (symbol->string name) steps #:style style-current) sub-protocols)

                  #;(sub-protocol "Sub protocol 1" '("do this" "do that" "..." "profit!") #:style style-current)
                  #;(sub-protocol (colorize #:color "green" "Sub protocol 2")
                                (list (WARNING "testing!")) #:style style-current)
                  #;(sub-protocol "Form testing"
                                (list
                                 (para (form ;#:action "." ;"somwhere-over-the-rainbow"
                                        (input #:name "yes" #:placeholder "a" #:label "measure 1"
                                               texin (linebreak))
                                        (input #:name "no" #:placeholder "b" #:label "measure 2"
                                               texin (linebreak))
                                        (input #:name "maybe" #:placeholder "c" #:label "measure 3"
                                               texin (linebreak))
                                        (input #:type "checkbox" box)
                                        (input #:type "submit" #:value "Submit")
                                        ;(button "Submit")
                                        ))))
                  ;(decode (list (paragraph sub-style "Executor: ")))
                  ;(simple-paragraph "Executor: ")
                  ;(simple-paragraph (~a "Execution date ____-__-__\n T __:__ " (current-timezone)))
                  #;(simple-part (tabular (apply zip-blank table-cols) #:sep (hspace 2)
                                          #:row-properties '(bottom-border ())
                                          #:style 'RBoxed))
                  ;)
          ])

      (list
       #;(decode (list (title #:style manual-doc-style "This is a thing")
                       (section "Metadata")
                       (subsection (~a "Execution date ____-__-__\n T __:__ " (current-timezone)))
                       ))
       (part tag-prefix
             tags
             title-content
             style-current
             to-collect
             blocks
             parts))))
  (values name scrib)
  )

(define (online-render docs
                       names
                       #:render-mixin [render-mixin html:render-mixin]
                       #:dest-dir [dest-dir #f]
                       #:helper-file-prefix [helper-file-prefix #f]
                       #:prefix-file [prefix-file #f]
                       #:style-file [style-file #f]
                       #:style-extra-files [style-extra-files null]
                       #:extra-files [extra-files null]
                       #:image-preferences [image-preferences null]
                       #:redirect [redirect #f]
                       #:redirect-main [redirect-main #f]
                       #:directory-depth [directory-depth 0]
                       #:xrefs [xrefs null]
                       #:info-in-files [info-input-files null]
                       #:info-out-file [info-output-file #f]
                       #:quiet? [quiet? #t]
                       #:warn-undefined? [warn-undefined? (not quiet?)]) 
  (let ([renderer (new (render-mixin render%)
                       [dest-dir dest-dir]
                       [prefix-file prefix-file]
                       [style-file style-file]
                       [style-extra-files style-extra-files]
                       [extra-files extra-files]
                       [image-preferences image-preferences]
                       [helper-file-prefix helper-file-prefix])])
    (when redirect
      (send renderer set-external-tag-path redirect))
    (when redirect-main
      (send renderer set-external-root-url redirect-main))
    (unless (zero? directory-depth)
      (send renderer set-directory-depth directory-depth))
    (unless quiet?
      (send renderer report-output!))
    (let* ([fns (map (lambda (fn)
                       (if fn 
                           (let-values ([(base name dir?) (split-path fn)])
                             (let ([fn (path-replace-suffix
                                        name
                                        (send renderer get-suffix))])
                               (if dest-dir (build-path dest-dir fn) fn)))
                           fn))
                     names)]
           [fp (send renderer traverse docs fns)]
           [info (send renderer collect docs fns fp)])
      (for ([file (in-list info-input-files)])
        (let ([s (with-input-from-file file read)])
          (send renderer deserialize-info s info)))
      (for ([xr (in-list xrefs)])
        (xref-transfer-info renderer info xr))
      (let ([r-info (send renderer resolve docs fns info)])
        (when warn-undefined?
          (let ([undef (send renderer get-undefined r-info)])
            (unless (null? undef)
              (eprintf "Warning: some cross references may be broken due to undefined tags:\n")
              (for ([t (in-list undef)])
                (eprintf " ~s\n" t)))))
        ; render all the things
        (send renderer render docs fns r-info)))))

(define (ast->html ast) 
  ; the html render mixin is the only one that
  ; matters for 'online' generation
  ; everything else is safe to do as 2 step
  (define-values (name parts-list) (protc->scribble ast))
  (define wat (online-render parts-list
                             (list #f)  ; renderer render outputs to string if filename is #f
                             #:render-mixin html:render-mixin))
  (car wat))

(define (get-user)
    ; NONE of the user information gathered at this point in the process can be trusted
    ; the proper time to validate who did what is at deposition time into a secure data
    ; server -- this could be as simple as having users sign a git commit hash with their
    ; private key
    ; NOTE anyone can pretend to be someone else by simply making a copy and changing the
    ; names or forging signatures etc, so unless the data has been deposited and the commit
    ; hash signed, the person with their name on the paper has denyability --
    ; NOTE in reverse, if someone signs data when someone else's name is on the document
    ; might want to flag since it could be someone trying to take credit for other's work
    ; though that is quite unlikley, a weird usecase is where there is division of labor
    ; and the scientist hands the paper to a typist or something, I think the proper way
    ; to implement this is by requiring the asserted contributor to sign off on the inclusion
    ; if the user submitting the data does not have the permissions/role to actually submit data
    ; (ie they only have permissions to enter data, not verify it)
    ; NOTE: /usr/bin/id is the best way to prevent trivial messing with user
    ; since we will also let users type these in unauthed this is kinda moot
    (let* (;[unix (environment-variables-ref (current-environment-variables) #"USER")] ; too easy to spoof
           [unix (system "/usr/bin/id -u -n")]
           ;[crypto-signature?]  ; TODO how to properly implement validating compile and execution... (cant)
           ; can only validate if they sign the hash of the output
           ; however, if this is running as a server then we can do this ...
           ;LDAP ...
           [config-name "Tom Gillespie"]  ; TODO
           [config-orcid "https://orcid.org/0000-0002-7509-4801"]
           #;[name (unix->name unix)])
      (user config-name config-orcid)))

(define-syntax (export stx)
  ; FIXME this is only intended to be used from inside a protc file...
  (syntax-parse stx
    #:datum-literals (.bind-vars .target)
    [(_ name:id   ; can also be a spec name implicitly? or maybe spec-name executor?
        (~optional (.bind-vars vals ...))   ; TODO make sure it matches impl-name .vars spec... (obvs)
        type:expr ... ; TODO programs
        )
     #:with name-ast (format-id #'name
                                #:source #'name
                                "~a-ast"
                                (syntax-e #'name))
     #'(begin
         (let-values ([(name scrib) (protc->scribble name-ast #:user (get-user))])
           (scribble->many #:name name scrib type ...)))
     ]
    ))

(define (export-file filename #:format [format 'html])
  (define to-ex
    #`(module export-stuff racket/base
        (require (file #,filename)
                 (only-in protc/export
                          get-user
                          protc->scribble
                          scribble->many))
        (for ([name-ast protc-for-export])
          (let-values ([(name scrib) (protc->scribble name-ast #:user (get-user))])
            (scribble->many #:name name scrib (quote #,format))))))
  ;(pretty-print (syntax->datum to-ex))
  ; make-empty-namespace produces weird #%paramz errors
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (eval-syntax to-ex)
    (dynamic-require ''export-stuff #f)))

#;(module+ test
    (ast->html '(data
                 ((.executor "human")
                  (.name my-protocol)
                  (.inputs (mouse vibratome))
                  (.outputs (brain-slice))
                  (.vars (slice-thickness))
                  (.measures (time-to-cell-death number-of-cells))
                  (other ("this is a body")))))
    )
