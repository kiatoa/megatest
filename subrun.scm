
;; Copyright 2006-2016, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

(use (prefix sqlite3 sqlite3:) srfi-1 posix regex regex-case srfi-69 (srfi 18) 
     posix-extras directory-utils pathname-expand typed-records format)
(declare (unit subrun))
;;(declare (uses runs))
;;(declare (uses db))
(declare (uses common))
;;(declare (uses items))
;;(declare (uses runconfig))
;;(declare (uses tests))
;;(declare (uses server))
;;(declare (uses mt))
;;(declare (uses archive))
;; (declare (uses filedb))

(include "common_records.scm")
;;(include "key_records.scm")
;;(include "db_records.scm")
;;(include "run_records.scm")
;;(include "test_records.scm")


(define (subrun:initialize-toprun-test  testconfig test-run-dir)

  (let ((ra (configf:lookup testconfig "subrun" "run-area"))
        (logpro (configf:lookup testconfig "subrun" "logpro")))
  (when (not ra)      ;; when runarea is not set we default to *toppath*. However 
              ;; we need to force the setting in the testconfig so it will
          ;; be preserved in the testconfig.subrun file
      (configf:set-section-var testconfig "subrun" "runarea" *toppath*))
    (configf:set-section-var testconfig "logpro" "subrun" logpro) ;; append the logpro rules to the logpro section as stepname subrun
    (configf:write-alist testconfig "testconfig.subrun")))


(define (subrun:launch-cmd test-run-dir)
  (let ((log-prefix "run")
        (switches (subrun:selector+log-switches test-run-dir log-prefix))
        (run-wait #t)
        (cmd      (conc "megatest -run "switches" "
                        (if runwait "-run-wait " ""))))
    cmd))

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
         (target        (or (alist-ref "-target" switch-alist  equal? #f) ;; want data-structures alist-ref, not alist-lib alist-ref
                            "NO-TARGET"))
         (runname       (or (alist-ref "-runname" switch-alist equal? #f)
                            "NO-RUNNAME"))
         (testpatt      (alist-ref "-testpatt" switch-alist equal? #f))
         (mode-patt     (alist-ref "-modepatt" switch-alist equal? #f))
         (tag-expr      (alist-ref "-tagexpr" switch-alist equal? #f))
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
     " -runname " runname " "
     " -target " target " "
     (if testpatt (conc "-testpatt " testpatt" ") "")
     (if modepatt (conc "-modepatt " modepatt" ") "")
     (if tag-expr (conc "-tag-expr " tag-expr" ") "")
     
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
                             


