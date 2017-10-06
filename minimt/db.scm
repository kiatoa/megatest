;; pretend to be a simplified Megatest

(use sql-de-lite defstruct)

;; init the db - NOTE: takes a db NOT a dbconn
;;
(define (init-db db)
  (with-transaction
   db
   (lambda ()
     (for-each
      (lambda (qrystr)
	(exec (sql db qrystr)))
      '("CREATE TABLE IF NOT EXISTS runs 
           (id        INTEGER PRIMARY KEY,
            target    TEXT NOT NULL,
            run_name  TEXT NOT NULL,
            state     TEXT NOT NULL,
            status    TEXT NOT NULL,
            CONSTRAINT runs_constraint UNIQUE (run_name));"
	"CREATE TABLE IF NOT EXISTS tests
           (id        INTEGER PRIMARY KEY,
            run_id    INTEGER NOT NULL,
            test_name TEXT NOT NULL,
            state     TEXT NOT NULL,
            status    TEXT NOT NULL,
            start_time INTEGER DEFAULT (strftime('%s','now')),
            end_time   INTEGER DEFAULT -1,
            CONSTRAINT tests_constraint UNIQUE (run_id,test_name));"
	"CREATE TABLE IF NOT EXISTS steps
           (id        INTEGER PRIMARY KEY,
            test_id   INTEGER NOT NULL,
            step_name  TEXT NOT NULL,
            state     TEXT NOT NULL,
            status    TEXT NOT NULL,
            CONSTRAINT step_constraint UNIQUE (test_id,step_name));")))))

(defstruct dbconn-dat
  dbh       ;; the database handle
  writeable ;; do we have write access?
  path      ;; where the db lives
  name      ;; name of the db
  )

;; open the database, return a dbconn struct
(define (open-create-db path fname init)
  (let* ((fullname       (conc path "/" fname))
	 (already-exists (file-exists? fullname))
	 (write-access   (and (file-write-access? path)
			      (or (not already-exists)
				  (and already-exists
				       (file-write-access? fullname)))))
	 (db             (if (or already-exists write-access)
			     (open-database fullname)
			     (begin
			       (print "FATAL: No existing db and no write access thus cannot create " fullname)  ;; no db and no write access cannot proceed.
			       (exit 1))))
	 (dbconn         (make-dbconn-dat)))
    (set-busy-handler! db (busy-timeout 120000)) ;; set a busy timeout
    (exec (sql db "PRAGMA synchronous=0;"))
    (if (and init write-access (not already-exists))
	(init db))
    (dbconn-dat-dbh-set!       dbconn db)
    (dbconn-dat-writeable-set! dbconn write-access)
    (dbconn-dat-path-set!      dbconn path)
    (dbconn-dat-name-set!      dbconn fname)
    dbconn))

(define-inline (get-db dbconn)
  (dbconn-dat-dbh dbconn))

;; RUNS

;; create a run
(define (create-run dbconn target run-name)
  (exec (sql (get-db dbconn) "INSERT INTO runs (run_name,target,state,status) VALUES (?,?,'NEW','na');")
	run-name target))

;; get a run id
(define (get-run-id dbconn target run-name)
  (first-column (query fetch (sql (get-db dbconn) "SELECT id FROM runs WHERE target=? AND run_name=?;")
		       target run-name)))

;; TESTS

(defstruct test-dat
  id
  run-id
  test-name
  state
  status)

;; create a test
(define (create-test dbconn run-id test-name)
  (exec (sql (get-db dbconn) "INSERT INTO tests (run_id,test_name,state,status) VALUES (?,?,'NOT_STARTED','na');")
	run-id test-name))

;; get a test id
(define (get-test-id dbconn run-id test-name)
  (first-column (query fetch (sql (get-db dbconn) "SELECT id FROM tests WHERE run_id=? AND test_name=?;")
		       run-id test-name)))

(define-inline (test-row->test-dat row)
    (make-test-dat
     id:        (list-ref row 0)
     run-id:    (list-ref row 1)
     test-name: (list-ref row 2)
     state:     (list-ref row 3)
     status:    (list-ref row 4)))
  
;; get the data for given test-id
(define (test-get-record dbconn test-id)
  (let* ((row (query fetch-row (sql (get-db dbconn) "SELECT id,run_id,test_name,state,status FROM tests WHERE test_id=?;")
		     test-id)))
    (test-row->test-dat row)))

;; get a bunch of tests data
(define (test-get-tests dbconn run-ids test-name-patt)
  (let* ((rows (query fetch-rows
		      (sql (get-db dbconn)
			   (conc "SELECT id,run_id,test_name,state,status FROM tests WHERE test_name LIKE ? AND run_id IN ("
				 (string-intersperse (map conc run-ids) ",") ");"))
		      test-name-patt)))
    (map test-row->test-dat rows)))
   
(define (test-set-state-status dbconn test-id new-state new-status)
  (exec (sql (get-db dbconn) "UPDATE tests SET state=?,status=?,end_time=? WHERE id=?;")
	new-state new-status (current-seconds) test-id))

;; STEPS

;; create a step
(define (create-step dbconn test-id step-name)
  (exec (sql (get-db dbconn) "INSERT INTO steps (test_id,step_name,state,status) VALUES (?,?,'NOT_STARTED','na');")
	test-id step-name))

;; get a step id
(define (get-step-id dbconn test-id step-name)
  (first-column (query fetch (sql (get-db dbconn) "SELECT id FROM steps WHERE test_id=? AND step_name=?;")
		       test-id step-name)))

(define (step-set-state-status dbconn step-id new-state new-status)
  (exec (sql (get-db dbconn) "UPDATE steps SET state=?,status=? WHERE id=?;")
	new-state new-status step-id))

;;======================================================================
;; Statistics gathering
;;======================================================================

(define *stats* (make-hash-table))

(define (update-stats key duration)
  (let ((rec (or (hash-table-ref/default *stats* key #f)
		 (let ((new (vector 0 0 0)))
		   (hash-table-set! *stats* key new)
		   new))))
    (vector-set! rec 0 (+ (vector-ref rec 0) 1))        ;; num calls
    (vector-set! rec 1 (+ (vector-ref rec 1) duration)) ;; total duration
    (if (> duration (vector-ref rec 2) )
	(vector-set! rec 2 duration))))

(define (statwrap name proc)
  (lambda params
    (let ((start-time (current-milliseconds))
	  (res        (apply proc params)))
      (update-stats name (- (current-milliseconds) start-time))
      res)))

(define (print-stats statdat)
  (hash-table-for-each
   statdat
   (lambda (key val)
     (print key " count: " (vector-ref val 0) " avg: " (/ (vector-ref val 1)(vector-ref val 0)) " max: " (vector-ref val 2)))))