;;======================================================================
;; dbstruct
;;======================================================================

;;
;; -path-|-megatest.db
;;       |-db-|-main.db
;;            |-monitor.db
;;            |-sdb.db
;;            |-fdb.db
;;            |-1.db
;;            |-<N>.db
;;
;;
;; Accessors for a dbstruct
;;

(define-inline (dbr:dbstruct-main    vec)    (vector-ref  vec 0)) ;; ( db path )
(define-inline (dbr:dbstruct-strdb   vec)    (vector-ref  vec 1)) ;; ( db path )
(define-inline (dbr:dbstruct-path    vec)    (vector-ref  vec 2)) 
(define-inline (dbr:dbstruct-local   vec)    (vector-ref  vec 3))
(define-inline (dbr:dbstruct-rundb   vec)    (vector-ref  vec 4)) ;; ( db path )
(define-inline (dbr:dbstruct-inmem   vec)    (vector-ref  vec 5)) ;; ( db #f )
(define-inline (dbr:dbstruct-mtime   vec)    (vector-ref  vec 6))
(define-inline (dbr:dbstruct-rtime   vec)    (vector-ref  vec 7))
(define-inline (dbr:dbstruct-stime   vec)    (vector-ref  vec 8))
(define-inline (dbr:dbstruct-inuse   vec)    (vector-ref  vec 9))
(define-inline (dbr:dbstruct-refdb   vec)    (vector-ref  vec 10)) ;; ( db path )
(define-inline (dbr:dbstruct-locdbs  vec)    (vector-ref  vec 11))
(define-inline (dbr:dbstruct-olddb   vec)    (vector-ref  vec 12)) ;; ( db path )
;; (define-inline (dbr:dbstruct-main-path vec)  (vector-ref  vec 13))
;; (define-inline (dbr:dbstruct-rundb-path vec) (vector-ref  vec 14))
;; (define-inline (dbr:dbstruct-run-id  vec)    (vector-ref  vec 13))

(define-inline (dbr:dbstruct-main-set!   vec val)(vector-set! vec 0 val))
(define-inline (dbr:dbstruct-strdb-set!  vec val)(vector-set! vec 1 val))
(define-inline (dbr:dbstruct-path-set!   vec val)(vector-set! vec 2 val))
(define-inline (dbr:dbstruct-local-set!  vec val)(vector-set! vec 3 val))
(define-inline (dbr:dbstruct-rundb-set!  vec val)(vector-set! vec 4 val))
(define-inline (dbr:dbstruct-inmem-set!  vec val)(vector-set! vec 5 val))
(define-inline (dbr:dbstruct-mtime-set!  vec val)(vector-set! vec 6 val))
(define-inline (dbr:dbstruct-rtime-set!  vec val)(vector-set! vec 7 val))
(define-inline (dbr:dbstruct-stime-set!  vec val)(vector-set! vec 8 val))
(define-inline (dbr:dbstruct-inuse-set!  vec val)(vector-set! vec 9 val))
(define-inline (dbr:dbstruct-refdb-set!  vec val)(vector-set! vec 10 val))
(define-inline (dbr:dbstruct-locdbs-set! vec val)(vector-set! vec 11 val))
(define-inline (dbr:dbstruct-olddb-set!  vec val)(vector-set! vec 12 val))
(define-inline (dbr:dbstruct-main-path-set! vec val)(vector-set! vec 13 val))
(define-inline (dbr:dbstruct-set-rupath-set! vec val)(vector-set! vec 14 val))

; (define-inline (dbr:dbstruct-run-id-set! vec val)(vector-set! vec 13 val))

;; constructor for dbstruct
;;
(define (make-dbr:dbstruct #!key (path #f)(local #f))
  (let ((v (make-vector 15 #f)))
    (dbr:dbstruct-path-set! v path)
    (dbr:dbstruct-local-set! v local)
    (dbr:dbstruct-locdbs-set! v (make-hash-table))
    v))

(define (dbr:dbstruct-localdb v run-id)
  (hash-table-ref/default (dbr:dbstruct-locdbs v) run-id #f))

(define (dbr:dbstruct-localdb-set! v run-id db)
  (hash-table-set! (dbr:dbstruct-locdbs v) run-id db))


(define (make-db:test)(make-vector 20))
(define-inline (db:test-id           vec) (vector-ref vec 0))
(define-inline (db:test-run_id       vec) (vector-ref vec 1))
(define-inline (db:test-testname     vec) (vector-ref vec 2))
(define-inline (db:test-state        vec) (vector-ref vec 3))
(define-inline (db:test-status       vec) (vector-ref vec 4))
(define-inline (db:test-event_time   vec) (vector-ref vec 5))
(define-inline (db:test-host         vec) (vector-ref vec 6))
(define-inline (db:test-cpuload      vec) (vector-ref vec 7))
(define-inline (db:test-diskfree     vec) (vector-ref vec 8))
(define-inline (db:test-uname        vec) (vector-ref vec 9))
;; (define-inline (db:test-rundir       vec) (sdb:qry 'getstr (vector-ref vec 10)))
(define-inline (db:test-rundir       vec) (vector-ref vec 10))
(define-inline (db:test-item-path    vec) (vector-ref vec 11))
(define-inline (db:test-run_duration vec) (vector-ref vec 12))
(define-inline (db:test-final_logf   vec) (vector-ref vec 13))
(define-inline (db:test-comment      vec) (vector-ref vec 14))
(define-inline (db:test-process_id   vec) (vector-ref vec 16))
(define-inline (db:test-archived     vec) (vector-ref vec 17))

;; (define-inline (db:test-pass_count   vec) (vector-ref vec 15))
;; (define-inline (db:test-fail_count   vec) (vector-ref vec 16))
(define-inline (db:test-fullname     vec)
  (conc (db:test-testname vec) "/" (db:test-item-path vec)))

;; replace runs:make-full-test-name with this routine
(define (db:test-make-full-name testname itempath)
  (if (equal? itempath "") testname (conc testname "/" itempath)))

(define-inline (db:test-first_err    vec) (printable (vector-ref vec 15)))
(define-inline (db:test-first_warn   vec) (printable (vector-ref vec 16)))

(define-inline (db:test-cpuload-set!  vec val)(vector-set! vec 7 val))
(define-inline (db:test-diskfree-set! vec val)(vector-set! vec 8 val))
(define-inline (db:test-testname-set! vec val)(vector-set! vec 2 val))
(define-inline (db:test-state-set!    vec val)(vector-set! vec 3 val))
(define-inline (db:test-status-set!   vec val)(vector-set! vec 4 val))
(define-inline (db:test-run_duration-set! vec val)(vector-set! vec 12 val))
(define-inline (db:test-final_logf-set! vec val)(vector-set! vec 13 val))

;; Test record utility functions

;; Is a test a toplevel?
;;
(define (db:test-is-toplevel vec)
  (and (equal? (db:test-item-path vec) "")      ;; test is not an item
       (equal? (db:test-uname vec)     "n/a"))) ;; test has never been run

;; make-vector-record "" db mintest id run_id testname state status event_time item_path
;;
(define (make-db:mintest)(make-vector 7))
(define-inline (db:mintest-get-id           vec)    (vector-ref  vec 0))
(define-inline (db:mintest-get-run_id       vec)    (vector-ref  vec 1))
(define-inline (db:mintest-get-testname     vec)    (vector-ref  vec 2))
(define-inline (db:mintest-get-state        vec)    (vector-ref  vec 3))
(define-inline (db:mintest-get-status       vec)    (vector-ref  vec 4))
(define-inline (db:mintest-get-event_time   vec)    (vector-ref  vec 5))
(define-inline (db:mintest-get-item_path    vec)    (vector-ref  vec 6))

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
(define-inline (db:test-data-get-type             vec)    (vector-ref  vec 10))

(define-inline (db:test-data-set-id!              vec val)(vector-set!  vec 0  val))
(define-inline (db:test-data-set-test_id!         vec val)(vector-set!  vec 1  val))
(define-inline (db:test-data-set-category!        vec val)(vector-set!  vec 2  val))
(define-inline (db:test-data-set-variable!        vec val)(vector-set!  vec 3  val))
(define-inline (db:test-data-set-value!           vec val)(vector-set!  vec 4  val))
(define-inline (db:test-data-set-expected!        vec val)(vector-set!  vec 5  val))
(define-inline (db:test-data-set-tol!             vec val)(vector-set!  vec 6  val))
(define-inline (db:test-data-set-units!           vec val)(vector-set!  vec 7  val))
(define-inline (db:test-data-set-comment!         vec val)(vector-set!  vec 8  val))
(define-inline (db:test-data-set-status!          vec val)(vector-set!  vec 9  val))
(define-inline (db:test-data-set-type!            vec val)(vector-set!  vec 10 val))

;;======================================================================
;; S T E P S 
;;======================================================================
;; Run steps
;; make-vector-record "Run steps" db step id test_id stepname step_complete step_pass event_time    
(define (make-db:step)(make-vector 7))
(define-inline (tdb:step-get-id              vec)    (vector-ref  vec 0))
(define-inline (tdb:step-get-test_id         vec)    (vector-ref  vec 1))
(define-inline (tdb:step-get-stepname        vec)    (vector-ref  vec 2))
(define-inline (tdb:step-get-state           vec)    (vector-ref  vec 3))
(define-inline (tdb:step-get-status          vec)    (vector-ref  vec 4))
(define-inline (tdb:step-get-event_time      vec)    (vector-ref  vec 5))
(define-inline (tdb:step-get-logfile         vec)    (vector-ref  vec 6))
(define-inline (tdb:step-set-id!             vec val)(vector-set! vec 0 val))
(define-inline (tdb:step-set-test_id!        vec val)(vector-set! vec 1 val))
(define-inline (tdb:step-set-stepname!       vec val)(vector-set! vec 2 val))
(define-inline (tdb:step-set-state!          vec val)(vector-set! vec 3 val))
(define-inline (tdb:step-set-status!         vec val)(vector-set! vec 4 val))
(define-inline (tdb:step-set-event_time!     vec val)(vector-set! vec 5 val))
(define-inline (tdb:step-set-logfile!        vec val)(vector-set! vec 6 val))


;; The steps table
(define (make-db:steps-table)(make-vector 5))
(define-inline (tdb:steps-table-get-stepname   vec)    (vector-ref  vec 0))
(define-inline (tdb:steps-table-get-start      vec)    (vector-ref  vec 1))
(define-inline (tdb:steps-table-get-end        vec)    (vector-ref  vec 2))
(define-inline (tdb:steps-table-get-status     vec)    (vector-ref  vec 3))
(define-inline (tdb:steps-table-get-runtime    vec)    (vector-ref  vec 4))
(define-inline (tdb:steps-table-get-log-file   vec)    (vector-ref  vec 5))

(define-inline (tdb:step-stable-set-stepname!  vec val)(vector-set! vec 0 val))
(define-inline (tdb:step-stable-set-start!     vec val)(vector-set! vec 1 val))
(define-inline (tdb:step-stable-set-end!       vec val)(vector-set! vec 2 val))
(define-inline (tdb:step-stable-set-status!    vec val)(vector-set! vec 3 val))
(define-inline (tdb:step-stable-set-runtime!   vec val)(vector-set! vec 4 val))

;; The data structure for handing off requests via wire
(define (make-cdb:packet)(make-vector 6))
(define-inline (cdb:packet-get-client-sig   vec)    (vector-ref  vec 0))
(define-inline (cdb:packet-get-qtype        vec)    (vector-ref  vec 1))
(define-inline (cdb:packet-get-immediate    vec)    (vector-ref  vec 2))
(define-inline (cdb:packet-get-query-sig    vec)    (vector-ref  vec 3))
(define-inline (cdb:packet-get-params       vec)    (vector-ref  vec 4))
(define-inline (cdb:packet-get-qtime        vec)    (vector-ref  vec 5))
(define-inline (cdb:packet-set-client-sig!  vec val)(vector-set! vec 0 val))
(define-inline (cdb:packet-set-qtype!       vec val)(vector-set! vec 1 val))
(define-inline (cdb:packet-set-immediate!   vec val)(vector-set! vec 2 val))
(define-inline (cdb:packet-set-query-sig!   vec val)(vector-set! vec 3 val))
(define-inline (cdb:packet-set-params!      vec val)(vector-set! vec 4 val))
(define-inline (cdb:packet-set-qtime!       vec val)(vector-set! vec 5 val))
