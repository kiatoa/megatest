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

(define-inline (dbr:dbstruct-get-main    vec)    (vector-ref  vec 0)) ;; ( db path )
(define-inline (dbr:dbstruct-get-strdb   vec)    (vector-ref  vec 1)) ;; ( db path )
(define-inline (dbr:dbstruct-get-path    vec)    (vector-ref  vec 2)) 
(define-inline (dbr:dbstruct-get-local   vec)    (vector-ref  vec 3))
(define-inline (dbr:dbstruct-get-rundb   vec)    (vector-ref  vec 4)) ;; ( db path )
(define-inline (dbr:dbstruct-get-inmem   vec)    (vector-ref  vec 5)) ;; ( db #f )
(define-inline (dbr:dbstruct-get-mtime   vec)    (vector-ref  vec 6))
(define-inline (dbr:dbstruct-get-rtime   vec)    (vector-ref  vec 7))
(define-inline (dbr:dbstruct-get-stime   vec)    (vector-ref  vec 8))
(define-inline (dbr:dbstruct-get-inuse   vec)    (vector-ref  vec 9))
(define-inline (dbr:dbstruct-get-refdb   vec)    (vector-ref  vec 10)) ;; ( db path )
(define-inline (dbr:dbstruct-get-locdbs  vec)    (vector-ref  vec 11))
(define-inline (dbr:dbstruct-get-olddb   vec)    (vector-ref  vec 12)) ;; ( db path )
;; (define-inline (dbr:dbstruct-get-main-path vec)  (vector-ref  vec 13))
;; (define-inline (dbr:dbstruct-get-rundb-path vec) (vector-ref  vec 14))
;; (define-inline (dbr:dbstruct-get-run-id  vec)    (vector-ref  vec 13))

(define-inline (dbr:dbstruct-set-main!   vec val)(vector-set! vec 0 val))
(define-inline (dbr:dbstruct-set-strdb!  vec val)(vector-set! vec 1 val))
(define-inline (dbr:dbstruct-set-path!   vec val)(vector-set! vec 2 val))
(define-inline (dbr:dbstruct-set-local!  vec val)(vector-set! vec 3 val))
(define-inline (dbr:dbstruct-set-rundb!  vec val)(vector-set! vec 4 val))
(define-inline (dbr:dbstruct-set-inmem!  vec val)(vector-set! vec 5 val))
(define-inline (dbr:dbstruct-set-mtime!  vec val)(vector-set! vec 6 val))
(define-inline (dbr:dbstruct-set-rtime!  vec val)(vector-set! vec 7 val))
(define-inline (dbr:dbstruct-set-stime!  vec val)(vector-set! vec 8 val))
(define-inline (dbr:dbstruct-set-inuse!  vec val)(vector-set! vec 9 val))
(define-inline (dbr:dbstruct-set-refdb!  vec val)(vector-set! vec 10 val))
(define-inline (dbr:dbstruct-set-locdbs! vec val)(vector-set! vec 11 val))
(define-inline (dbr:dbstruct-set-olddb!  vec val)(vector-set! vec 12 val))
(define-inline (dbr:dbstruct-set-main-path! vec val)(vector-set! vec 13 val))
(define-inline (dbr:dbstruct-set-rundb-path! vec val)(vector-set! vec 14 val))

; (define-inline (dbr:dbstruct-set-run-id! vec val)(vector-set! vec 13 val))

;; constructor for dbstruct
;;
(define (make-dbr:dbstruct #!key (path #f)(local #f))
  (let ((v (make-vector 15 #f)))
    (dbr:dbstruct-set-path! v path)
    (dbr:dbstruct-set-local! v local)
    (dbr:dbstruct-set-locdbs! v (make-hash-table))
    v))

;; Returns the database for a particular run-id fron the dbstruct:localdbs
;;
(define (dbr:dbstruct-get-localdb v run-id)
  (hash-table-ref/default (dbr:dbstruct-get-locdbs v) run-id #f))

(define (dbr:dbstruct-set-localdb! v run-id db)
  (hash-table-set! (dbr:dbstruct-get-locdbs v) run-id db))

(require-extension typed-records)
(defstruct db:test-rec ((id -1) : number)
					((run_id -1) : number) 
					((testname "") : string)
					((state "") : string)
					((status "") : string)
					((event_time -1) : number)
					((host "") : string)
					((cpuload -1) : number)
					((diskfree -1) : number)
					((uname "") : string)
					((rundir "") : string)
					((item_path "") : string)
					((run_duration -1) : number)
					((final_logf "") : string)
					((comment "") : string)
					((process-id -1) : number)
					((archived -1) : number)
					((shortdir -1) : number)
					((attemptnum -1) : number))

"id"           "run_id"        "testname"  "state"      "status"      "event_time"
				"host"         "cpuload"       "diskfree"  "uname"      "rundir"      "item_path"
                                "run_duration" "final_logf"    "comment"   "shortdir"   "attemptnum"  "archived"

(define (db:qry-gen-alist qrystr listvals)
	(define listqry (string-split qrystr ","))
	(if (null? listqry)
	      '()
	      (let loop ((strhead (car listqry))
			 (strtail (cdr listqry))
			 (valhead (car listvals))
			 (valtail (cdr listvals))
			 (res '()))
		(let* ((slot-val-pair (cons (string->symbol strhead) valhead)))
		  (if (or (null? strtail)
		  		(null? valtail))
		      (cons slot-val-pair res);;(print strhead valhead));;(cons (cons (string->symbol strhead) valhead) res))
		      (loop (car strtail)(cdr strtail)(car valtail)(cdr valtail)(cons slot-val-pair res)))))))

(define (db:test-get-id			typed-rec)   (db:test-rec-id 		typed-rec))
(define (db:test-get-run_id 	typed-rec)	 (db:test-rec-run_id 	typed-rec))
(define (db:test-get-testname   typed-rec)   (db:test-rec-testname typed-rec))
(define (db:test-get-state      typed-rec)   (db:test-rec-state 	typed-rec))
(define (db:test-get-status     typed-rec)   (db:test-rec-status 	typed-rec))
(define (db:test-get-event_time typed-rec)   (db:test-rec-event_time typed-rec))
(define (db:test-get-host       typed-rec)   (db:test-rec-host 		typed-rec))
(define (db:test-get-cpuload    typed-rec)   (db:test-rec-cpuload 	typed-rec))
(define (db:test-get-diskfree   typed-rec)   (db:test-rec-diskfree 	typed-rec))
(define (db:test-get-uname      typed-rec)   (db:test-rec-uname 	typed-rec))
(define (db:test-get-rundir     typed-rec)   (db:test-rec-rundir 	typed-rec))
(define (db:test-get-item-path  typed-rec)   (db:test-rec-item_path typed-rec))
(define (db:test-get-run_duration typed-rec)  (db:test-rec-run_duration typed-rec))
(define (db:test-get-final_logf typed-rec)   (db:test-rec-final_logf typed-rec))
(define (db:test-get-comment    typed-rec)   (db:test-rec-comment 	typed-rec))
(define (db:test-get-process_id typed-rec)   (db:test-rec-process-id typed-rec))
(define (db:test-get-archived   typed-rec)   (db:test-rec-archived 	typed-rec))

;; (define-inline (db:test-get-pass_count   vec) (vector-ref vec 15))
;; (define-inline (db:test-get-fail_count   vec) (vector-ref vec 16))
(define-inline (db:test-get-fullname     vec)
  (conc (db:test-get-testname vec) "/" (db:test-get-item-path vec)))

;; replace runs:make-full-test-name with this routine
(define (db:test-make-full-name testname itempath)
  (if (equal? itempath "") testname (conc testname "/" itempath)))

(define-inline (db:test-get-first_err    vec) (printable (vector-ref vec 15)))
(define-inline (db:test-get-first_warn   vec) (printable (vector-ref vec 16))) ;; RADT => reference 16 is repeated

(define-inline (db:test-set-cpuload!  vec val)		(db:test-rec-cpuload-set! vec val))
(define-inline (db:test-set-diskfree! vec val)		(db:test-rec-diskfree-set! vec val))
(define-inline (db:test-set-testname! vec val)		(db:test-rec-testname-set! vec val))
(define-inline (db:test-set-state!    vec val)		(db:test-rec-state-set! vec val))
(define-inline (db:test-set-status!   vec val)		(db:test-rec-status-set! vec val))
(define-inline (db:test-set-run_duration! vec val)	(db:test-rec-run_duration-set! vec val))
(define-inline (db:test-set-final_logf! vec val)	(db:test-rec-final_logf-set! vec val))

;; Test record utility functions

;; Is a test a toplevel?
;;
(define (db:test-get-is-toplevel vec)
  (and (equal? (db:test-get-item-path vec) "")      ;; test is not an item
       (equal? (db:test-get-uname vec)     "n/a"))) ;; test has never been run

;; make-vector-record "" db mintest id run_id testname state status event_time item_path
;; RADT => purpose of mintest??
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
(define-inline (tdb:step-get-comment         vec)    (vector-ref  vec 7))
(define-inline (tdb:step-set-id!             vec val)(vector-set! vec 0 val))
(define-inline (tdb:step-set-test_id!        vec val)(vector-set! vec 1 val))
(define-inline (tdb:step-set-stepname!       vec val)(vector-set! vec 2 val))
(define-inline (tdb:step-set-state!          vec val)(vector-set! vec 3 val))
(define-inline (tdb:step-set-status!         vec val)(vector-set! vec 4 val))
(define-inline (tdb:step-set-event_time!     vec val)(vector-set! vec 5 val))
(define-inline (tdb:step-set-logfile!        vec val)(vector-set! vec 6 val))
(define-inline (tdb:step-set-comment!        vec vak)(vector-set! vec 7 val))


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
