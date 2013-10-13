(use srfi-18 srfi-69)

(define args (argv))

(if (not (eq? (length args) 2))
    (begin
      (print "Usage: sqlitecompare [insert|update]")
      (exit 0)))

(define action (string->symbol (cadr args)))

(system "rm -f test.db")

(define test-table-defn
		 "CREATE TABLE IF NOT EXISTS tests 
                    (id INTEGER PRIMARY KEY,
                     run_id     INTEGER,
                     testname   TEXT,
                     host       TEXT DEFAULT 'n/a',
                     cpuload    REAL DEFAULT -1,
                     diskfree   INTEGER DEFAULT -1,
                     uname      TEXT DEFAULT 'n/a', 
                     rundir     TEXT DEFAULT 'n/a',
                     shortdir   TEXT DEFAULT '',
                     item_path  TEXT DEFAULT '',
                     state      TEXT DEFAULT 'NOT_STARTED',
                     status     TEXT DEFAULT 'FAIL',
                     attemptnum INTEGER DEFAULT 0,
                     final_logf TEXT DEFAULT 'logs/final.log',
                     logdat     BLOB, 
                     run_duration INTEGER DEFAULT 0,
                     comment    TEXT DEFAULT '',
                     event_time TIMESTAMP,
                     fail_count INTEGER DEFAULT 0,
                     pass_count INTEGER DEFAULT 0,
                     archived   INTEGER DEFAULT 0, -- 0=no, 1=in progress, 2=yes
                     CONSTRAINT testsconstraint UNIQUE (run_id, testname, item_path)
          );")

(define test-insert "INSERT INTO tests  (run_id,testname,host,cpuload,diskfree,uname,rundir,shortdir,item_path,state,status,final_logf,run_duration,comment,event_time)
                                values (?,     ?,       ?,   ?,      ?,       ?,    ?,     ?,       ?,        ?,    ?,     ?,         ?,           ?,      ?        );")

(define tests '("test0" "test1" "test2" "test3" "test4" "test5" "test6" "test7" "test8" "test9"))
(define items '())
(for-each 
 (lambda (n)
   (for-each 
    (lambda (m)
      (set! items (cons (conc "item/" n m) items)))
    '(0 1 2 3 4 5 6 7 8 9)))
 '(0 1 2 3 4 5 6 7 8 9))
(define hosts '("host0" "host1" "host2" "host3" "host4" "host5" "host6" "host7" "host8" "host9"))
(define cpuloads '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))
(define diskfrees '(100000 200000 300000 400000 500000 600000 700000 800000 900000))
(define uname "Linux xena 3.5.0-40-generic #62~precise1-Ubuntu SMP Fri Aug 23 17:59:10 UTC 2013 i686 i686 i386 GNU/Linux")
(define basedir "/mfs/matt/data/megatest/runs/testing")
(define final-logf "finallog.html")
(define run-durations (list 120 240 260))
(define comments '("" "this is a good one eh?" "this one sucks eh?" "just eh, eh?"))

(define run-ids (make-hash-table))
(define max-run-id 1000)

(define (test-factors->run-id host cpuload diskfree run-duration comment)
  (let* ((factor (conc host "-" cpuload "-" diskfree "-" run-duration "-" comment))
	 (run-id (hash-table-ref/default run-ids factor #f)))
    (if run-id 
	(list run-id factor)
	(let ((new-id (+ max-run-id 1)))
	  (set! max-run-id new-id)
	  (hash-table-set! run-ids factor new-id)
	  (list new-id factor)))))
	  

(define (create-tests db)
  (for-each 
   (lambda (test)
     (for-each 
      (lambda (item)
	(for-each 
	 (lambda (host)
	   (for-each 
	    (lambda (cpuload)
	      (for-each
	       (lambda (diskfree)
		 (for-each 
		  (lambda (run-duration)
		    (for-each 
		     (lambda (comment)
		       (let* ((run-id-dat (test-factors->run-id host cpuload diskfree run-duration comment))
			      (run-id (car run-id-dat))
			      (factor (cadr run-id-dat)))
			 (print "Adding " run-id " " test " " item " " factor)
			 (register-test db  
					run-id
					test  ;; testname
					host
					cpuload
					diskfree
					uname
					(conc basedir "/" test "/" item) ;; rundir
					(conc test "/" item) ;; shortdir
					item   ;; item-path
					"NOT_STARTED" ;; state
					"NA"          ;; status
					final-logf
					run-duration
					comment
					(current-seconds))))
		     comments))
		  run-durations))
	       diskfrees))
	    cpuloads))
	 hosts))
      items))
   tests))

				   
			 