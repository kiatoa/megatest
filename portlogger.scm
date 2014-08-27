

;; Copyright 2006-2014, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n)

(use sqlite3 srfi-1 posix srfi-69 hostinfo)
(import (prefix sqlite3 sqlite3:))

;; lsof -i


(define (portlogger:open-db fname)
  (let* ((exists   (file-exists? fname))
	 (db       (sqlite3:open-database fname))
	 (handler  (make-busy-timeout 136000))
	 (canwrite (file-write-access? fname)))
    (sqlite3:set-busy-handler! db handler)
    (sqlite3:execute db "PRAGMA synchronous = 0;")
    (sqlite3:execute 
     db
     "CREATE TABLE ports (
        port INTEGER PRIMARY KEY,
        state TEXT DEFAULT 'not-used',
        fail_count INTEGER DEFAULT 0);")
    db))

;; (fold-row PROC INIT DATABASE SQL . PARAMETERS) 
(define (portlogger:take-port db portnum)
  (let* ((qry1 (sqlite3:prepare "INSERT INTO ports (port,state) VALUES (?,?);"))
	 (qry2 (sqlite3:prepare "UPDATE ports SET state=? WHERE port=?;"))
	 (qry3 (sqlite3:prepare "SELECT state FROM ports WHERE port=?;"))
	 (res  (sqlite3:with-transaction
		db
		(lambda ()
		  ;; (fold-row (lambda (var curr) (or var curr)) #f db "SELECT var FROM foo WHERE id=100;")
		  (let ((curr (sqlite3:fold-row
			       (lambda (var curr)
				 (or var curr))
			       "not-tried"
			       qry3
			       portnum))
			(res   (case (string->symbol curr)
				 ((released)  (sqlite3:execute qry2 "taken" portnum) 'taken)
				 ((not-tried) (sqlite3:execute qry1 portnum "taken") 'taken)
				 ((taken)                                            'already-taken)
				 ((failed)                                           'failed)
				 (else                                               'error))))
		    res)))))
    (sqlite3:finalize! qry1)
    (sqlite3:finalize! qry2)
    (sqlite3:finalize! qry3)
    res))
       