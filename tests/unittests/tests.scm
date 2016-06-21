;; ;;======================================================================
;; ;; itemwait, itemmatch
;; 
;; (db:compare-itempaths ref-item-path item-path itemmap)
;; 
;; ;; prereqs-not-met
;; 
;; (rmt:get-prereqs-not-met run-id waitons item-path mode: testmode itemmap: itemmap))
;; 
;; 	 (fails           (runs:calc-fails prereqs-not-met))
;; 	 (prereq-fails    (runs:calc-prereq-fail prereqs-not-met))
;; 	 (non-completed   (runs:calc-not-completed prereqs-not-met))
;; 	 (runnables       (runs:calc-runnable prereqs-not-met)))
;; 
;; 
;; 

(define user    (current-user-name))
(define runname "mytestrun")
(define keys    (rmt:get-keys))
(define runinfo #f)
(define keyvals '(("SYSTEM" "abc")("RELEASE" "def")))
(define header  (list "SYSTEM" "RELEASE" "id" "runname" "state" "status" "owner" "event_time"))
(define run-id  1)

;; Create a run
(test #f 1 (rmt:register-run keyvals runname "new" "n/a" user))
(test #f #t (rmt:general-call 'register-test run-id run-id "test-one"   ""))
(test #f #t (rmt:general-call 'register-test run-id run-id "test-two"   ""))
(test #f #t (rmt:general-call 'register-test run-id run-id "test-three" ""))
(test #f #t (rmt:general-call 'register-test run-id run-id "test-four"  ""))

(rmt:test-set-state-status-by-id run-id (rmt:get-test-id run-id "test-one"   "") "COMPLETED" "FAIL" "")
(rmt:test-set-state-status-by-id run-id (rmt:get-test-id run-id "test-two"   "") "COMPLETED" "PASS" "")
(rmt:test-set-state-status-by-id run-id (rmt:get-test-id run-id "test-three" "") "RUNNING"   "n/a"  "")
(rmt:test-set-state-status-by-id run-id (rmt:get-test-id run-id "test-four"  "") "COMPLETED" "WARN" "")

(print "MODE=not in")
(test #f '()
      (filter
       (lambda (y)
	 (equal? y "FAIL")) ;; any FAIL in the output list?
       (map 
	(lambda (x)(vector-ref x 4))
	(rmt:get-tests-for-run run-id "%/%" '() '("FAIL") #f #f #t 'event_time "DESC" 'shortlist 0 'dashboard))))

(print "MODE=in")
(test #f '("FAIL")
      (map 
       (lambda (x)(vector-ref x 4))
       (rmt:get-tests-for-run run-id "%/%" '() '("FAIL") #f #f #f 'event_time "DESC" 'shortlist 0 'dashboard)))
(set! *verbosity* 1)

(print "MODE=in, state in RUNNING")
;; (set! *verbosity* 8)
(test #f '("RUNNING")
      (map 
       (lambda (x)(vector-ref x 3))
       (rmt:get-tests-for-run run-id "%/%" '("RUNNING") '() #f #f #f 'event_time "DESC" 'shortlist 0 'dashboard)))
(set! *verbosity* 1)

(print "MODE=in, state in RUNNING and status IN WARN")
;; (set! *verbosity* 8)
(test #f '(("RUNNING" . "n/a") ("COMPLETED" . "WARN"))
      (map 
       (lambda (x)
	 (cons (vector-ref x 3)(vector-ref x 4)))
       (rmt:get-tests-for-run run-id "%/%" '("RUNNING") '("WARN") #f #f #f 'event_time "DESC" 'shortlist 0 'dashboard)))
(set! *verbosity* 1)

(print "MODE=not in, state in RUNNING and status IN WARN")
(set! *verbosity* 8)
(test #f '(("RUNNING" . "n/a") ("COMPLETED" . "WARN"))
      (map 
       (lambda (x)
	 (cons (vector-ref x 3)(vector-ref x 4)))
       (rmt:get-tests-for-run run-id "%/%" '("RUNNING") '("WARN") #f #f #t 'event_time "DESC" 'shortlist 0 'dashboard)))
(set! *verbosity* 1)

(exit)
