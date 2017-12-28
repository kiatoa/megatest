
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
     posix-extras directory-utils pathname-expand typed-records format
     call-with-environment-variables)
(declare (unit subrun))
;;(declare (uses runs))
(declare (uses db))
(declare (uses common))
;;(declare (uses items))
;;(declare (uses runconfig))
;;(declare (uses tests))
;;(declare (uses server))
(declare (uses mt))
;;(declare (uses archive))
;; (declare (uses filedb))

;(include "common_records.scm")
;;(include "key_records.scm")
(include "db_records.scm") ;; provides db:test-get-id
;;(include "run_records.scm")
;;(include "test_records.scm")

(define (subrun:subrun-test-initialized? test-run-dir)
  (if (and (common:file-exists? (conc test-run-dir "/subrun-area") )
           (common:file-exists? (conc test-run-dir "/testconfig.subrun") ))
      #t
      #f))

(define (subrun:subrun-removed? test-run-dir)
  (if (subrun:subrun-test-initialized? test-run-dir)
      (let ((flagfile (conc test-run-dir "/subrun.removed")))
        (if (common:file-exists? flagfile)
            #t
            #f))
      #t))

(define (subrun:set-subrun-removed test-run-dir)
  (let ((flagfile (conc test-run-dir "/subrun.removed")))
    (if (and (subrun:subrun-test-initialized? test-run-dir) (not (common:file-exists? flagfile)))
        (with-output-to-file flagfile
          (lambda () (print (current-seconds)))))))

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


(define (subrun:remove-subrun test-run-dir keep-records )
;; set state/status of test item
;; fork off megatest
;; set state/status of test item
;;
  ;;(BB> "Entered subrun:remove-subrun with "test-fulln)
  (if (and (not (subrun:subrun-removed? test-run-dir)) (subrun:subrun-test-initialized? test-run-dir))
      (let* ((action-switches-str
              (conc "-remove-runs"
                    (if keep-records "-keep-records " "")
                    ))
             (remove-result
              (subrun:exec-sub-megatest test-run-dir action-switches-str "remove")))
        (if remove-result
            (begin
              (subrun:set-subrun-removed test-run-dir)
              #t)
            #f))
      #t))

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

(define (subrun:exec-sub-megatest test-run-dir action-switches-str log-prefix)
  (let* ((selector-switches  (subrun:selector+log-switches test-run-dir log-prefix))
         (cmd (conc "megatest " selector-switches " " action-switches-str ))
         (pid #f)
         (proc (lambda ()
                 (debug:print-info 0 *default-log-port* "Running sub megatest command: "cmd)
                 ;;(set! pid (process-run "/usr/bin/xterm" (list ))))))
                 (set! pid (process-run "/bin/bash" (list "-c" cmd))))))
    (call-with-environment-variables 
     (list (cons "PATH" (conc (get-environment-variable "PATH") ":.")))
     (lambda  ()
       (common:without-vars proc "^MT_.*")))
    (let processloop ((i 0))
      (let-values (((pid-val exit-status exit-code)(process-wait pid #t)))
        (if (eq? pid-val 0)
            (begin
              (thread-sleep! 2)
              (processloop (+ i 1)))
            (begin
              (debug:print-info 0 *default-log-port* "sub megatest " action-switches-str " completed with exit code " exit-code)
              (if (eq? 0 exit-code)
                  (begin
                    #t)
                  (begin
                    #f))))))))



;; (subrun:exec-sub-megatest "/nfs/pdx/disks/icf_env_disk001/bjbarcla/gwa/issues/mtdev/165/megatest/ext-tests/tests/subrun-usecases/toparea/links/SYSTEM_val/RELEASE_val/go/toptest" "-foo" "foo")
