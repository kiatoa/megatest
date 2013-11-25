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
(define (make-dbr:dbstruct #!key (path #f))
  (vector
   #f                  ;; the main db (contains runs, test_meta etc.) NOT CACHED IN MEM
   (make-hash-table)   ;; run-id => [ rundb inmemdb last-mod last-read last-sync ]
   #f                  ;; the global string db (use for state, status etc.)
   path))              ;; path to database files/megatest area

;; get and set main db
(define-inline (dbr:dbstruct-get-main vec)    (vector-ref vec 0))
(define-inline (dbr:dbstruct-set-main! vec db)(vector-set! vec 0 db))

;; get a rundb vector
(define (dbr:dbstruct-get-rundb-rec vec run-id)
  (let* ((dbhash (vector-ref vec 1))
	 (runvec (hash-table-ref/default dbhash run-id)))
    (if runvec
	runvec
	(begin
	  (hash-table-set! dbhash run-id (vector #f #f -1 -1 -1 #f))
	  (dbr:dbstruct-get-rundb-rec vec run-id)))))

;;  [ rundb inmemdb last-mod last-read last-sync ]
(define-inline (dbr:dbstruct-field-name->num field-name)
  (case field-name
    ((rundb) 0) ;; the on-disk db
    ((inmem) 1) ;; the in-memory db
    ((mtime) 2) ;; last modification time
    ((rtime) 3) ;; last read time
    ((stime) 4) ;; last sync time
    ((inuse) 5) ;; is the db currently in use
    (else -1)))

;; get/set rundb fields
(define (dbr:dbstruct-get-runrec vec run-id field-name)
  (let ((runvec   (dbr:dbstruct-get-rundb-rec vec run-id))
	(fieldnum (dbr:dbstruct-field-name->num field-name)))
    ;; (vector-set! runvec (dbr:dbstruct-field-name->num 'inuse) #t)
    (vector-ref runvec fieldnum)))

(define (dbr:dbstruct-set-runvec! vec run-id field-name val)
  (let ((runvec (dbr:dbstruct-get-rundb-rec vec run-id)))
    (vector-set! runvec (dbr:dbstruct-field-name->num field-name) rundb)))

;; get/set inmemdb
(define (dbr:dbstruct-get-inmemdb vec run-id)
  (let ((runvec (dbr:dbstruct-get-rundb-rec vec run-id)))
    (vector-ref runvec 1)))

(define (dbr:dbstruct-set-inmemdb! vec run-id inmemdb)
  (let ((runvec (dbr:dbstruct-get-rundb-rec vec run-id)))
    (vector-set! runvec 1 inmemdb)))

;; the string db
(define-inline (dbr:dbstruct-get-strdb vec)    (vector-ref vec 2))
(define-inline (dbr:dbstruct-set-strdb! vec db)(vector-set! vec 2 db))

;; path
(define-inline (dbr:dbstruct-set-path! vec path)(vector-set! vec 3))
(define-inline (dbr:dbstruct-get-path  vec)     (vector-ref vec 3))


(define (make-db:test)(make-vector 20))
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
(define-inline (db:test-get-pass_count   vec) (vector-ref vec 15))
(define-inline (db:test-get-fail_count   vec) (vector-ref vec 16))
(define-inline (db:test-get-fullname     vec)
  (conc (db:test-get-testname vec) "/" (db:test-get-item-path vec)))

(define-inline (db:test-get-first_err    vec) (printable (vector-ref vec 15)))
(define-inline (db:test-get-first_warn   vec) (printable (vector-ref vec 16)))

(define-inline (db:test-set-cpuload!  vec val)(vector-set! vec 7 val))
(define-inline (db:test-set-diskfree! vec val)(vector-set! vec 8 val))
(define-inline (db:test-set-testname! vec val)(vector-set! vec 2 val))
(define-inline (db:test-set-state!    vec val)(vector-set! vec 3 val))
(define-inline (db:test-set-status!   vec val)(vector-set! vec 4 val))
(define-inline (db:test-set-run_duration! vec val)(vector-set! vec 12 val))
(define-inline (db:test-set-final_logf! vec val)(vector-set! vec 13 val))

;; get rows and header from 
(define-inline (db:get-header vec)(vector-ref vec 0))
(define-inline (db:get-rows   vec)(vector-ref vec 1))

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
(define-inline (tdb:step-stable-set-stepname!  vec val)(vector-set! vec 0 val))
(define-inline (tdb:step-stable-set-start!     vec val)(vector-set! vec 1 val))
(define-inline (tdb:step-stable-set-end!       vec val)(vector-set! vec 2 val))
(define-inline (tdb:step-stable-set-status!    vec val)(vector-set! vec 3 val))
(define-inline (tdb:step-stable-set-runtime!   vec val)(vector-set! vec 4 val))

;; use this one for db-get-run-info
(define-inline (db:get-row    vec)(vector-ref vec 1))

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
