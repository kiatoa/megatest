

(define (subrun:initialize-toprun-test test-run-dir testconfig)
  (let ((ra (configf:lookup testconfig "subrun" "run-area")))
    (when (not ra)      ;; when runarea is not set we default to *toppath*. However 
              ;; we need to force the setting in the testconfig so it will
          ;; be preserved in the testconfig.subrun file
      (configf:set-section-var testconfig "subrun" "runarea" *toppath*)
      )

  (configf:write-alist testconfig "testconfig.subrun") 
  )


(define (subrun:launch )



  )

;; set state/status of test item
;; fork off megatest
;; set state/status of test item
;;


(define (subrun:selector+log-switches test-run-dir log-prefix)
  (let* ((switch-def-alist (common:get-param-mapping flavor: 'config))
         (subrunfile   (conc test-run-dir "/testconfig.subrun" ))
         (subrundata   (with-input-from-file subrunfile read))
         (subrunconfig (configf:alist->config subrundata))
         (run-area     (configf:lookup subrunconfig "subrun" "run-area"))
         (defvals      `(("-runname" . ,(get-environment-variable "MT_RUNNAME"))
                         ("-target"  . ,(get-environment-variable "MT_TARGET"))))
         (switch-alist (apply
                        append
                        (filter-map (lambda (item)
                                      (let ((config-key (car item))
                                            (switch     (cdr item))
                                            (defval     (alist-ref defvals switch equal?))
                                            (val        (or (configf:lookup subrunconfig switch)
                                                            defval)))
                                        (if val
                                            (list switch val)
                                            #f)))
                                    switch-def-alist)))
         (target        (or (alist-ref switch-alist "-target" equal?)
                            "NO-TARGET"))
         (runname       (or (alist-ref switch-alist "-runname" equal?)
                            "NO-RUNNAME"))
         (testpatt      (alist-ref switch-alist "-testpatt" equal?))
         (mode-patt     (alist-ref switch-alist "-modepatt" equal?))
         (tag-expr      (alist-ref switch-alist "-tagexpr" equal?))
         (compact-stem  (string-substitute "[/*]" "_"
                                           (conc
                                            target
                                            "-"
                                            runname
                                            "-" (or testpatt mode-patt tag-expr "NO-TESTPATT"))))
         (logfile       (conc
                         test-run-dir "/"
                         (or log-prefix "")
                         (if log-prefix "-" "")
                         compact-stem
                         ".log")))
    ;; note - get precmd from subrun section
    ;;   apply to submegatest commands
    
    (conc
     " -start-dir " run-area " "
     
     (string-intersperse
      (apply append
           (map (lambda (x) (list (car x) (cdr x))) switch-def-alist))
      " ")
     "-log " logfile)))


(define (subrun:exec-sub-megatest test-run-dir switches #!key (logfile #f))
  (let* ((real-logfile (or logfile (conc (test-run-dir) "/subrun-"
                                         (string-substitute "[/*]" "_" (string-intersperse switches "^"))"-"
                                         (number->string (current-seconds)) ".log")))
         (selector-switches  (common:sub-megatest-selector-switches test-run-dir))
         (cmd-list `("megatest" ,@selector-switches ,@switches "-log" ,real-logfile))
         )
    (call-with-environment-variables 
     (list (cons "PATH" (conc (get-environment-variable "PATH") ":.")))
     (lambda  ()
       (common:without-vars proc "^MT_.*")
       
       ))))
                             


