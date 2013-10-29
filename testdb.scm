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
;; Test Database access
;;======================================================================

(require-extension (srfi 18) extras)

(use sqlite3 srfi-1 posix regex regex-case srfi-69 csv-xml s11n md5 message-digest base64)
(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))

;; Note, try to remove this dependency 
;; (use zmq)

(declare (unit testdb))
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
;; Functions to access test db files with some caching of handles
;;======================================================================

(define (db:get-db dbstruct run-id)
  (let ((db (if run-id
		(hash-table-ref/default (vector-ref dbstruct 1) run-id #f)
		(vector-ref dbstruct 0))))
    (if db
	db
	(let ((db (open-db run-id)))
	  (if run-id
	      (hash-table-set! (vector-ref dbstruct 1) run-id db)
	      (vector-set! dbstruct 0 db))
	  db))))

;;======================================================================
;; K E E P   F I L E D B   I N   dbstruct
;;======================================================================

(define (db:get-filedb dbstruct)
  (let ((db (vector-ref dbstruct 2)))
    (if db
	db
	(let ((fdb (filedb:open-db (conc *toplevel* "/db/files.db"))))
	  (vector-set! dbstruct 2 fdb)
	  fdb))))

;; Can also be used to save arbitrary strings
;;
(define (db:save-path dbstruct path)
  (let ((fdb (db:get-filedb dbstruct)))
    (filedb:register-path fdb path)))

;; Use to get a path. To get an arbitrary string see next define
;;
(define (db:get-path dbstruct id)
  (let ((fdb (db:get-filedb dbstruct)))
    (filedb:get-path db id)))

;;======================================================================
;;
;; U S E   F I L E   D B   T O   S T O R E   S T R I N G S 
;;
;; N O T E ! !   T H I S   C L O B B E R S   M U L T I P L E  ////  T O  /
;;
;; Replace with something proper!
;;
;;======================================================================

;; Use to save a stored string, pad with _ to deal with trimming the prepending of /
;; 
(define (db:save-string dbstruct str)
  (let ((fdb (db:get-filedb dbstruct)))
    (filedb:register-path fdb (conc "_" str))))

;; Use to get a stored string
;;
(define (db:get-string dbstruct id)
  (let ((fdb (db:get-filedb dbstruct)))
    (string-drop (filedb:get-path fdb id) 2)))

;; This routine creates the db. It is only called if the db is not already opened
;;
(define (open-db dbstruct test-id) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: Attempted to open db when not in megatest area. Exiting.")
	    (exit))))


REWORKING open-db


  (let* ((test-rec     (db:test-id->record test-id))
	 (dbpath       (conc (db:test-get-test-path test-rec) "/testdat.db"))


	 (dbexists     (file-exists? dbpath))
	 (write-access (file-write-access? dbpath))
	 (db           (sqlite3:open-database dbpath)) ;; (never-give-up-open-db dbpath))
	 (handler      (make-busy-timeout (if (args:get-arg "-override-timeout")
					      (string->number (args:get-arg "-override-timeout"))
					      136000)))) ;; 136000))) ;; 136000 = 2.2 minutes
    (if (and dbexists
	     (not write-access))
	(set! *db-write-access* write-access)) ;; only unset so other db's also can use this control
    (debug:print-info 11 "open-db, dbpath=" dbpath " argv=" (argv))
    (sqlite3:set-busy-handler! db handler)
    (if (not dbexists)
	(if (not run-id) ;; do the megatest.db
	    (db:initialize-megatest-db db)
	    (db:initialize-run-id-db   db run-id)))
    (sqlite3:execute db "PRAGMA synchronous = 0;")
    db))

;; close all opened run-id dbs
(define (db:close-all-db)
  (for-each
   (lambda (db)
     (finalize! db))
   (hash-table-values (vector-ref *open-dbs* 1)))
  (finalize! (vector-ref *open-dbs* 0)))

