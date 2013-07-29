;;======================================================================
;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

;;======================================================================
;; Database access
;;======================================================================

(require-extension (srfi 18) extras tcp) ;;  rpc)
;; (import (prefix rpc rpc:))

(use sqlite3 srfi-1 posix regex regex-case srfi-69 csv-xml s11n md5 message-digest base64)
(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))

;; Note, try to remove this dependency 
;; (use zmq)

(declare (unit tdb))
(declare (uses common))
(declare (uses keys))
(declare (uses ods))
(declare (uses fs-transport))
(declare (uses client))
(declare (uses mt))

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")
(include "run_records.scm")

;;======================================================================
;;
;; T E S T   D A T A B A S E S
;;
;;======================================================================

(define (tdb:get-steps-data tdb test-id)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id test-id stepname state status event-time logfile)
       (set! res (cons (vector id test-id stepname state status event-time (if (string? logfile) logfile "")) res)))
     tdb
     "SELECT id,test_id,stepname,state,status,event_time,logfile FROM test_steps WHERE test_id=? ORDER BY id ASC;" ;; event_time DESC,id ASC;
     test-id)
    (sqlite3:finalize! tdb)
    (reverse res)))

(define (tdb:read-test-data tdb test-id categorypatt)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id test_id category variable value expected tol units comment status type)
       (set! res (cons (vector id test_id category variable value expected tol units comment status type) res)))
     tdb
     "SELECT id,test_id,category,variable,value,expected,tol,units,comment,status,type FROM test_data WHERE test_id=? AND category LIKE ? ORDER BY category,variable;" test-id categorypatt)
    (sqlite3:finalize! tdb)
    (reverse res)))
