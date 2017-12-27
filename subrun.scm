
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

;(include "common_records.scm")
;;(include "key_records.scm")
;;(include "db_records.scm")
;;(include "run_records.scm")
;;(include "test_records.scm")

(define (subrun:subrun-test-initialized? test-run-dir)
  (if (and (common:file-exists? (conc test-run-dir "/subrun-area") )
           (common:file-exists? (conc test-run-dir "/testconfig.subrun") ))
      #t
      #f))

(define (subrun:testconfig-defines-subrun? testconfig)
  (configf:lookup testconfig "subrun" "runwait")) ;; we use runwait as the flag that a subrun is requested

(define (subrun:initialize-toprun-test  testconfig test-run-dir)

  (let ((ra (configf:lookup testconfig "subrun" "run-area"))
        (logpro (configf:lookup testconfig "subrun" "logpro"))
        (symlink-target (conc test-run-dir "/subrun-area"))
        )
  (when (not ra)      ;; when runarea is not set we default to *toppath*. However 
              ;; we need to force the setting in the testconfig so it will
          ;; be preserved in the testconfig.subrun file
      (configf:set-section-var testconfig "subrun" "runarea" *toppath*))
    (configf:set-section-var testconfig "logpro" "subrun" logpro) ;; append the logpro rules to the logpro section as stepname subrun

    (if (common:file-exists? symlink-target)
        (delete-file symlink-target))
    
    (create-symbolic-link ra symlink-target)

    (configf:write-alist testconfig "testconfig.subrun")))


(define (subrun:remove-subrun test-run-dir new-test-dat test-name item-path test-state test-fulln toplevel-with-children test)
;; set state/status of test item
;; fork off megatest
;; set state/status of test item
;;

  (let* ((subrun-alist (subrun:selector+log-alist test-run-dir log-prefix))
         (runlog       (alist-ref "-log" subrun-alist equal? #f)))
    (if (not (common:file-exists? runlog))
        (BB> "no runlog @ "runlog)
        (if (member test-state (list "RUNNING" "LAUNCHED" "REMOTEHOSTSTART" "KILLREQ"))
            ;; This test is not in a correct state for cleaning up. Let's try some graceful shutdown steps first
            ;; Set the test to "KILLREQ" and wait five seconds then try again. Repeat up to five times then give
            ;; up and blow it away.
            
            ;; call in submegatest:
            ;;  (tasks:kill-runner target run-name testpatt)
            
            (mt:test-set-state-status-by-id run-id (db:test-get-id test) "SUBRUN-KILLREQ" "n/a" #f)
            )

        ;; on success:
        ;;   set state of test, or delete it or whatever
        )
    )
  )

(define (subrun:launch-cmd test-run-dir)
  (let* ((log-prefix "run")
         (switches (subrun:selector+log-switches test-run-dir log-prefix))
         (run-wait #t)
         (cmd      (conc "megatest -run "switches" "
                         (if run-wait "-run-wait " ""))))
    cmd))


(define (subrun:selector+log-alist test-run-dir log-prefix)
  (let* ((switch-def-alist (common:get-param-mapping flavor: 'config))
         (subrunfile   (conc test-run-dir "/testconfig.subrun" ))
         (subrundata   (with-input-from-file subrunfile read))
         (subrunconfig (configf:alist->config subrundata))
         (run-area     (configf:lookup subrunconfig "subrun" "run-area"))
         (defvals      `(("start-dir" . ,(or run-area  ;; default values if not specified in subrun section of tconf
                                             (get-environment-variable "MT_RUN_AREA_HOME")
                                             "/no/rundir/found")) 
                         ("run-name"  . ,(or (get-environment-variable "MT_RUNNAME") "NO-RUNNAME"))
                         ("target"    . ,(or (get-environment-variable "MT_TARGET")  "NO-TARGET"))))
         (switch-alist-pre  (filter-map (lambda (item)
                                          (let* ((config-key (car item))
                                                 (switch     (cdr item))
                                                 (defval     (alist-ref config-key defvals equal? #f))
                                                 (val        (or (configf:lookup subrunconfig "subrun" config-key)
                                                                 defval)))
                                            (if val
                                                (cons switch val)
                                                #f)))
                                        switch-def-alist))

         ;; testpatt may be modified if all three of mode-patt, tag-expr, and testpatt are null
         (mode-patt     (alist-ref "-modepatt" switch-alist-pre equal? #f))
         (tag-expr      (alist-ref "-tagexpr" switch-alist-pre equal? #f))
         (testpatt      (alist-ref "-testpatt" switch-alist-pre equal?
                                   (if (not (or mode-patt tag-expr)) "%" #f))) ;; testpatt is % if not
                                                                               ;; otherwise specified

         ;; define compact-stem for logfile
         (target        (alist-ref "-target" switch-alist-pre equal? #f)) ;; want data-structures alist-ref, not alist-lib alist-ref
         (runname       (alist-ref "-runname" switch-alist-pre equal? #f))


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
                         ".log"))
         ;; swap out testpatt with modified test-patt and add -log
         (switch-alist  (cons
                         (cons "-log" logfile)
                         (map (lambda (item)
                                (if (equal? (car item) "-testpatt")
                                    (cons "-testpatt" testpatt)
                                    item))
                                switch-alist-pre))))
    switch-alist))
    ;; note - get precmd from subrun section
    ;;   apply to submegatest commands

(define (subrun:get-log-path test-run-dir log-prefix)
  (let* ((alist (subrun:selector+log-alist test-run-dir log-prefix))
         (res   (alist-ref "-log" alist equal? #f)))
    res))

(define (subrun:selector+log-switches test-run-dir log-prefix)
  (let* ((switch-alist (subrun:selector+log-alist test-run-dir log-prefix))
         (res
          (string-intersperse
           (apply
            append
            (map
             (lambda (x)
               (list (car x) (cdr x)))
             switch-alist))
           " ")))
    res))

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
                             


