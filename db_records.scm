(define (make-db:test)(make-vector 6))
(define-inline (db:test-get-id           vec) (vector-ref vec 0))
(define-inline (db:test-get-run_id       vec) (vector-ref vec 1))
(define-inline (db:test-get-testname     vec) (vector-ref vec 2))
(define-inline (db:test-get-state        vec) (vector-ref vec 3))
(define-inline (db:test-get-status       vec) (vector-ref vec 4))
(define-inline (db:test-get-event_time   vec) (vector-ref vec 5))
(define-inline (db:test-get-host         vec) (vector-ref vec 6))
(define-inline (db:test-get-cpuload      vec) (vector-ref vec 7))
(define-inline (db:test-get-diskfree     vec) (vector-ref vec 8))
(define-inline (db:test-get-uname        vec) (vector-ref vec 9))
(define-inline (db:test-get-rundir       vec) (vector-ref vec 10))
(define-inline (db:test-get-item-path    vec) (vector-ref vec 11))
(define-inline (db:test-get-run_duration vec) (vector-ref vec 12))
(define-inline (db:test-get-final_logf   vec) (vector-ref vec 13))
(define-inline (db:test-get-comment      vec) (vector-ref vec 14))
(define-inline (db:test-get-fullname     vec)
  (conc (db:test-get-testname vec) "/" (db:test-get-item-path vec)))
;; (define-inline (db:test-get-value        vec) (printable (vector-ref vec 15)))
;; (define-inline (db:test-get-expected_value vec)(printable (vector-ref vec 16)))
;; (define-inline (db:test-get-tol          vec) (printable (vector-ref vec 17)))
;; (define-inline (db:test-get-units        vec) (printable (vector-ref vec 15))) ;; 18)))
(define-inline (db:test-get-first_err    vec) (printable (vector-ref vec 15))) ;; 19)))
(define-inline (db:test-get-first_warn   vec) (printable (vector-ref vec 16))) ;; 20)))

(define-inline (db:test-set-testname! vec val)(vector-set! vec 2 val))
(define-inline (db:test-set-state!    vec val)(vector-set! vec 3 val))
(define-inline (db:test-set-status!   vec val)(vector-set! vec 4 val))

;; get rows and header from 
(define-inline (db:get-header vec)(vector-ref vec 0))
(define-inline (db:get-rows   vec)(vector-ref vec 1))

;; make-vector-record db testmeta id testname author owner description reviewed iterated avg_runtime avg_disk
(define (make-db:testmeta)(make-vector 10 ""))
(define-inline (db:testmeta-get-id            vec)    (vector-ref  vec 0))
(define-inline (db:testmeta-get-testname      vec)    (vector-ref  vec 1))
(define-inline (db:testmeta-get-author        vec)    (vector-ref  vec 2))
(define-inline (db:testmeta-get-owner         vec)    (vector-ref  vec 3))
(define-inline (db:testmeta-get-description   vec)    (vector-ref  vec 4))
(define-inline (db:testmeta-get-reviewed      vec)    (vector-ref  vec 5))
(define-inline (db:testmeta-get-iterated      vec)    (vector-ref  vec 6))
(define-inline (db:testmeta-get-avg_runtime   vec)    (vector-ref  vec 7))
(define-inline (db:testmeta-get-avg_disk      vec)    (vector-ref  vec 8))
(define-inline (db:testmeta-get-tags          vec)    (vector-ref  vec 9))
(define-inline (db:testmeta-set-id!           vec val)(vector-set! vec 0 val))
(define-inline (db:testmeta-set-testname!     vec val)(vector-set! vec 1 val))
(define-inline (db:testmeta-set-author!       vec val)(vector-set! vec 2 val))
(define-inline (db:testmeta-set-owner!        vec val)(vector-set! vec 3 val))
(define-inline (db:testmeta-set-description!  vec val)(vector-set! vec 4 val))
(define-inline (db:testmeta-set-reviewed!     vec val)(vector-set! vec 5 val))
(define-inline (db:testmeta-set-iterated!     vec val)(vector-set! vec 6 val))
(define-inline (db:testmeta-set-avg_runtime!  vec val)(vector-set! vec 7 val))
(define-inline (db:testmeta-set-avg_disk!     vec val)(vector-set! vec 8 val))

;;======================================================================
;; T E S T   D A T A 
;;======================================================================
(define (make-db:test-data)(make-vector 10))
(define-inline (db:test-data-get-id               vec)    (vector-ref  vec 0))
(define-inline (db:test-data-get-test_id          vec)    (vector-ref  vec 1))
(define-inline (db:test-data-get-category         vec)    (vector-ref  vec 2))
(define-inline (db:test-data-get-variable         vec)    (vector-ref  vec 3))
(define-inline (db:test-data-get-value            vec)    (vector-ref  vec 4))
(define-inline (db:test-data-get-expected         vec)    (vector-ref  vec 5))
(define-inline (db:test-data-get-tol              vec)    (vector-ref  vec 6))
(define-inline (db:test-data-get-units            vec)    (vector-ref  vec 7))
(define-inline (db:test-data-get-comment          vec)    (vector-ref  vec 8))
(define-inline (db:test-data-get-status           vec)    (vector-ref  vec 9))

;;======================================================================
;; S T E P S 
;;======================================================================
;; Run steps
;; make-vector-record "Run steps" db step id test_id stepname step_complete step_pass event_time    
(define (make-db:step)(make-vector 7))
(define-inline (db:step-get-id              vec)    (vector-ref  vec 0))
(define-inline (db:step-get-test_id         vec)    (vector-ref  vec 1))
(define-inline (db:step-get-stepname        vec)    (vector-ref  vec 2))
(define-inline (db:step-get-state           vec)    (vector-ref  vec 3))
(define-inline (db:step-get-status          vec)    (vector-ref  vec 4))
(define-inline (db:step-get-event_time      vec)    (vector-ref  vec 5))
(define-inline (db:step-get-logfile         vec)    (vector-ref  vec 6))
(define-inline (db:step-set-id!             vec val)(vector-set! vec 0 val))
(define-inline (db:step-set-test_id!        vec val)(vector-set! vec 1 val))
(define-inline (db:step-set-stepname!       vec val)(vector-set! vec 2 val))
(define-inline (db:step-set-state!          vec val)(vector-set! vec 3 val))
(define-inline (db:step-set-status!         vec val)(vector-set! vec 4 val))
(define-inline (db:step-set-event_time!     vec val)(vector-set! vec 5 val))
(define-inline (db:step-set-logfile!        vec val)(vector-set! vec 6 val))


;; The steps table
(define (make-db:steps-table)(make-vector 5))
(define-inline (db:steps-table-get-stepname   vec)    (vector-ref  vec 0))
(define-inline (db:steps-table-get-start      vec)    (vector-ref  vec 1))
(define-inline (db:steps-table-get-end        vec)    (vector-ref  vec 2))
(define-inline (db:steps-table-get-status     vec)    (vector-ref  vec 3))
(define-inline (db:steps-table-get-runtime    vec)    (vector-ref  vec 4))
(define-inline (db:step-stable-set-stepname!  vec val)(vector-set! vec 0 val))
(define-inline (db:step-stable-set-start!     vec val)(vector-set! vec 1 val))
(define-inline (db:step-stable-set-end!       vec val)(vector-set! vec 2 val))
(define-inline (db:step-stable-set-status!    vec val)(vector-set! vec 3 val))
(define-inline (db:step-stable-set-runtime!   vec val)(vector-set! vec 4 val))

;; use this one for db-get-run-info
(define-inline (db:get-row    vec)(vector-ref vec 1))

