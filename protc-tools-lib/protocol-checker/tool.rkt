#lang racket/base
; for simple example see git://drracket/drracket/browser
; for example see git://drracket/drracket/macro-debugger
; docs https://docs.racket-lang.org/tools/implementing-tools.html

(require racket/unit
         racket/gui/base
         racket/class
         framework
         framework/preferences
         mrlib/switchable-button
         drracket/tool
         images/compile-time
         (for-syntax racket/base images/icons/tool images/icons/style)
         protc/protcheck)

(provide tool@)

(define-local-member-name allow-protocol-checker?)
(define-local-member-name run-protocol-checker)

(define frame/supports-protocol-checker<%>
  (interface ()
             allow-protocol-checker?
             run-protocol-checker))


; needed to prevent error messages
(define pref:close-on-reset-console? (preferences:get/set 'protcheck:CloseOnResetConsole?))

(define drracket-macro-stepper-director%
  (class object% ;macro-stepper-director/process%
    (init-field filename)
    (inherit-field stepper-frames)
    (define eventspace (current-eventspace))

    (define stepper #f)
    (inherit new-stepper)

    (define/private (lazy-new-stepper)
      (unless stepper
        (set! stepper (new-stepper))))

    (define/override (add-trace events)
      (parameterize ((current-eventspace eventspace))
        (queue-callback
         (lambda ()
           (lazy-new-stepper)
           (super add-trace events)))))
    (define/override (add-deriv deriv)
      (parameterize ((current-eventspace eventspace))
        (queue-callback
         (lambda ()
           (lazy-new-stepper)
           (super add-deriv deriv)))))

    #;
    (define/override (new-stepper-frame)
      (parameterize ((current-eventspace eventspace))
        (new macro-stepper-frame%
             (config (new macro-stepper-config/prefs%))
             (filename filename)
             (director this))))

    (define/public (shutdown)
      (when (pref:close-on-reset-console?)
        (for ([(frame flags) (in-hash stepper-frames)])
          (unless (memq 'no-obsolete flags)
            (send frame show #f)))))

    (super-new)))

(define protocol-checker-button-label "Protocol Checker")

; from syncheck-drracket-button.rkt
(define protcheck-bitmap
  (compiled-bitmap (check-syntax-icon #:height (toolbar-icon-height))))
(define protcheck-small-bitmap
  (compiled-bitmap (small-check-syntax-icon #:height (toolbar-icon-height))))

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) 
      (drracket:module-language-tools:add-opt-in-toolbar-button
        (λ (frame parent)
           (new switchable-button%
                (label protocol-checker-button-label)
                (bitmap protcheck-bitmap)
                (alternate-bitmap protcheck-small-bitmap)
                (parent parent)
                (callback (λ (button) (send frame protcheck:button-callback)))))
        'protc:protcheck
        #:number 50)
      ;(drracket:unit:add-to-program-editor-mixin clearing-text-mixin)
      (drracket:language:register-capability
       'protocol-checker:enabled
       boolean?
       #f))
    ;(define (phase2) (void))
    (define (phase2) (message-box "test 2" "phase 2"))

    (define drracket-eventspace (current-eventspace))
    (define drracket-custodian (current-custodian))

    (define (protocol-checker-unit-frame-mixin %)
      (class* % (frame/supports-protocol-checker<%>)
        (super-new)
        (inherit get-button-panel
                 get-language-menu
                 get-interactions-text
                 get-definitions-text
                 get-top-level-window
                 ensure-rep-hidden
                 get-current-tab)

        (define protocol-check-panel
          (new panel:horizontal-discrete-sizes%
               (parent (get-button-panel))))
        (define protocol-check-button
          (new switchable-button%
               (label protocol-checker-button-label)
               (bitmap protcheck-bitmap)
               (alternate-bitmap protcheck-small-bitmap)
               (parent protocol-check-panel)
               (callback (lambda (button) (protcheck:button-callback)))))
        (inherit register-toolbar-button)
        (register-toolbar-button protocol-check-button #:number 70)

        (define/augment (enable-evaluation)
          (send protocol-check-button enable #t)
          (inner (void) enable-evaluation))
        (define/augment (disable-evaluation)
          (send protocol-check-button enable #f)
          (inner (void) disable-evaluation))

        (define protocol-check-menu-item
          (let ([lang-menu (get-language-menu)])
            (new separator-menu-item% (parent lang-menu))
            (new menu-item%
                 (label "Protocol Checker")
                 (parent lang-menu)
                 (callback (lambda _ (protcheck:button-callback))))))
        ;; Hide button for inappropriate languages

        (define/augment (on-tab-change old new)
          (check-language)
          (inner (void) on-tab-change old new))

        (define/public (check-language)
          (enable/disable-stuff (allow-protocol-checker?)))

        (define/public (allow-macro-stepper?)
          (let ([lang
                 (drracket:language-configuration:language-settings-language
                  (send (get-definitions-text) get-next-settings))])
            (send lang capability-value 'protocol-checker:enabled)))

        (define/private (enable/disable-stuff enable?)
          (if enable?
            #t
            #;
              (begin (send macro-debug-menu-item enable #t)
                     (unless (send macro-debug-button is-shown?)
                       (send macro-debug-panel
                             add-child macro-debug-button)))
              #f
            #;
              (begin (send macro-debug-menu-item enable #f)
                     (when (send macro-debug-button is-shown?)
                       (send macro-debug-panel
                             delete-child macro-debug-button)))))

        (send (get-button-panel) change-children
              (lambda (_)
                (cons macro-debug-panel
                      (remq macro-debug-panel _))))
        (check-language)

        (define/public-final (protcheck:button-callback)
          (ensure-rep-hidden)
          ;; FIXME!!! Lots of this is copied out of drracket/macro-stepper/tool.rkt
          ;; FIXME!!! Lots of this is copied out of drracket/private/syncheck/gui.rkt
          ;; except some of the code (eg error handling) thrown away to avoid pulling
          ;; in lots more. Need to abstract.

          (define definitions-text (get-definitions-text))
          (define interactions-text (get-interactions-text))
          (define drs-eventspace (current-eventspace))
          (define drs-custodian (current-custodian))
          (define the-tab (get-current-tab))
          (define-values (old-break-thread old-custodian) (send the-tab get-breakables))
          (define error-port (send (send the-tab get-error-report-text) get-err-port))
          (define output-port (send (send the-tab get-error-report-text) get-out-port))

          (define user-custodian #f)
          (define normal-termination? #f)
          (define original-module-name-resolver #f)

          (define director
            (parameterize ((current-eventspace drracket-eventspace)
                           (current-custodian drracket-custodian))
              (let ([filename (send definitions-text get-filename/untitled-name)])
                (new drracket-macro-stepper-director% (filename filename)))))
          (send interactions-text set-macro-stepper-director director)

          (define (the-module-name-resolver . args)
            (void)
            #;(parameterize ((current-expand-observe void))
              #;(apply original-module-name-resolver args)))

          ;; --

          #;
          (define (init-proc) ;; =user=
            (set! original-module-name-resolver (current-module-name-resolver))
            (current-module-name-resolver the-module-name-resolver)

            (send the-tab set-breakables (current-thread) (current-custodian))
            (set-directory definitions-text)
            (current-load-relative-directory #f)
            (current-error-port error-port)
            (current-output-port output-port)
            (error-display-handler 
             (λ (msg exn) ;; =user=
               (parameterize ([current-eventspace drs-eventspace])
                 (queue-callback
                  (λ () ;; =drs=
                    ;; this has to come first or else the positioning
                    ;; computations in the highlight-errors/exn method
                    ;; will be wrong by the size of the error report box
                    (show-error-report/tab)
                    ;; a call like this one also happens in 
                    ;; drracket:debug:error-display-handler/stacktrace
                    ;; but that call won't happen here, because
                    ;; the rep is not in the current-rep parameter
                    (send interactions-text highlight-errors/exn exn))))
               (drracket:debug:error-display-handler/stacktrace
                msg exn '()
                #:definitions-text definitions-text)
               (semaphore-post error-display-semaphore)))
            (error-print-source-location #f) ; need to build code to render error first
            (uncaught-exception-handler
             (let ([oh (uncaught-exception-handler)])
               (λ (exn)
                 (uncaught-exception-raised)
                 (oh exn))))
            (set! user-custodian (current-custodian)))

          (define (uncaught-exception-raised) ;; =user=
            (set! normal-termination? #t)
            (parameterize ([current-eventspace drs-eventspace])
              (queue-callback
               (λ ()
                 (cleanup)
                 (custodian-shutdown-all user-custodian)))))
          (define (show-error-report/tab) ;; =drs=
            (send the-tab turn-on-error-report)
            (send (send the-tab get-error-report-text) scroll-to-position 0)
            (when (eq? (get-current-tab) the-tab)
              ;; (show-error-report)
              (void)))
          (define (cleanup) ;; =drs=
            (send the-tab set-breakables old-break-thread old-custodian)
            (send the-tab enable-evaluation)
            ;; do this with some lag ... not great, but should be okay.
            (let ([err-port (send (send the-tab get-error-report-text) get-err-port)])
              (thread
               (λ ()
                 (flush-output err-port)
                 (queue-callback
                  (λ ()
                    (unless (= 0 (send (send the-tab get-error-report-text) last-position))
                      (show-error-report/tab))))))))
          (define (kill-termination)
            (unless normal-termination?
              (parameterize ([current-eventspace drs-eventspace])
                (queue-callback
                 (λ ()
                   (cleanup)
                   (custodian-shutdown-all user-custodian))))))

          #;
          (with-lock/edit-sequence definitions-text
            (lambda ()
              (send the-tab clear-annotations)
              (send the-tab reset-offer-kill)
              (define get-terms
                (drracket:eval:traverse-program/multiple
                 settings init-proc kill-termination
                 #:gui-modules? #f))
              (get-terms
               (drracket:language:make-text/pos definitions-text
                                                0
                                                (send definitions-text last-position))
               (λ (sexp loop) ; =user=
                 (cond [(eof-object? sexp)
                        (set! normal-termination? #t)
                        (parameterize ([current-eventspace drs-eventspace])
                          (queue-callback
                           (λ () ; =drs=
                             (cleanup)
                             (custodian-shutdown-all user-custodian))))]
                       [(syntax? sexp)
                        (let-values ([(e-expr events derivp) (expand+trace sexp)])
                          (send director add-trace events)
                          (cond [(syntax? e-expr)
                                 ;; FIXME: eval compile-time parts?
                                 (void)]
                                [else (raise e-expr)]))
                        (loop)]
                       [else
                        (eprintf "Got non-syntax: ~e" sexp)
                        (loop)]))
               #t)))

          (void))

        ))

          (message-box "test" "unit running?")
          (drracket:get/extend:extend-unit-frame protocol-checker-unit-frame-mixin)))
