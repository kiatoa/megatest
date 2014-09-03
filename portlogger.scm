
;; Copyright 2006-2014, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n)

(use sqlite3 srfi-1 posix srfi-69 hostinfo dot-locking)
(import (prefix sqlite3 sqlite3:))

(declare (unit portlogger))

;; lsof -i


(define (portlogger:open-db fname)
  (let* ((avail    (tasks:wait-on-journal fname 10)) ;; wait up to about 10 seconds for the journal to go away
	 (exists   (file-exists? fname))
	 (db       (sqlite3:open-database fname))
	 (handler  (make-busy-timeout 136000))
	 (canwrite (file-write-access? fname)))
    (sqlite3:set-busy-handler! db handler)
    (sqlite3:execute db "PRAGMA synchronous = 0;")
    (if (not exists)
	(sqlite3:execute 
	 db
	 "CREATE TABLE ports (
            port INTEGER PRIMARY KEY,
            state TEXT DEFAULT 'not-used',
            fail_count INTEGER DEFAULT 0,
            update_time TIMESTAMP DEFAULT (strftime('%s','now')) );"))
    db))

(define (portlogger:open-run-close proc . params)
  (let* ((fname  (conc "/tmp/." (current-user-name) "-portlogger.db"))
	 (avail  (tasks:wait-on-journal fname 10))) ;; wait up to about 10 seconds for the journal to go away
    (handle-exceptions
     exn
     (begin
       ;; (release-dot-lock fname)
       (debug:print 0 "ERROR: portlogger:open-run-close failed. " proc " " params)
       (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
       (debug:print 0 "exn=" (condition->list exn))
       (print-call-chain))
     (let* (;; (lock   (obtain-dot-lock fname 2 9 10))
	    (db     (portlogger:open-db fname))
	    (res    (apply proc db params)))
       (sqlite3:finalize! db)
       ;; (release-dot-lock fname)
       res))))

;; (fold-row PROC INIT DATABASE SQL . PARAMETERS) 
(define (portlogger:take-port db portnum)
  (let* ((qry1 (sqlite3:prepare db "INSERT INTO ports (port,state) VALUES (?,?);"))
	 (qry2 (sqlite3:prepare db "UPDATE ports SET state=?,update_time=strftime('%s','now') WHERE port=?;"))
	 (qry3 (sqlite3:prepare db "SELECT state FROM ports WHERE port=?;"))
	 (res  (sqlite3:with-transaction
		db
		(lambda ()
		  ;; (fold-row (lambda (var curr) (or var curr)) #f db "SELECT var FROM foo WHERE id=100;")
		  (let* ((curr #f)
			 (res  #f))
		    (set! curr (sqlite3:fold-row
				(lambda (var curr)
				  (or curr var curr))
				"not-tried"
				qry3
				portnum))
		    ;; (print "curr=" curr)
		    (set! res (case (string->symbol curr)
				((released)  (sqlite3:execute qry2 "taken" portnum) 'taken)
				((not-tried) (sqlite3:execute qry1 portnum "taken") 'taken)
				((taken)                                            'already-taken)
				((failed)                                           'failed)
				(else                                               'error)))
		    ;; (print "res=" res)
		    res)))))
    (sqlite3:finalize! qry1)
    (sqlite3:finalize! qry2)
    (sqlite3:finalize! qry3)
    res))

(define (portlogger:get-prev-used-port db)
  (sqlite3:fold-row
   (lambda (var curr)
     (or curr var curr))
   #f
   db
   "SELECT (port) FROM ports WHERE state='released' LIMIT 1;"))

(define (portlogger:find-port db)
  (let* ((lowport (let ((val (configf:lookup *configdat* "server" "lowport")))
		    (if (and val 
			     (string->number val))
			(string->number val)
			32768)))
	 (portnum (or (portlogger:get-prev-used-port db)
		      (+ lowport ;; top of registered ports is 49152 but lets use ports in the registered range
			 (random (- 64000 lowport))))))
    (portlogger:take-port db portnum)
    portnum))

;; set port to "released", "failed" etc.
;; 
(define (portlogger:set-port db portnum value)
  (sqlite3:execute db "UPDATE ports SET state=?,update_time=strftime('%s','now') WHERE port=?;" value portnum))

;; set port to failed (attempted to take but got error)
;;
(define (portlogger:set-failed db portnum)
  (sqlite3:execute db "UPDATE ports SET state='failed',fail_count=fail_count+1,update_time=strftime('%s','now') WHERE port=?;" portnum))

;;======================================================================
;; MAIN
;;======================================================================



(define (portlogger:main . args)
  (let* ((db      (portlogger:open-db (conc "/tmp/." (current-user-name) "-portlogger.db")))
	 (numargs (length args))
	 (result  (cond
		   ((> numargs 1) ;; most commands
		    (case (string->symbol (car args)) ;; commands with two or more params
		      ((take)(portlogger:take-port db (string->number (cadr args))))
		      ((set) (portlogger:set-port db 
						  (string->number (cadr args))
						  (caddr args))
		       (caddr args))
		      ((failed)(portlogger:set-failed db (string->number (cadr args))) 'failed))))))
    (sqlite3:finalize! db)
    result))
     
;; (print (apply portlogger:main (cdr (argv))))
