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
;; Simple persistant strings lookup table. Keep out of the main db
;; so writes/reads don't slow down central access.
;;======================================================================

(require-extension (srfi 18) extras)
(use sqlite3 srfi-1 posix regex regex-case srfi-69 csv-xml s11n md5 message-digest base64)
(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))

(declare (unit sdb))

;; 
(define (sdb:open #!key (fname #f)) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: Attempted to open db when not in megatest area. Exiting.")
	    (exit))))
  (let* ((dbpath    (conc *toppath* "/db/" (if fname fname "sdb.db"))) ;; fname)
	 (dbexists  (let ((fe (file-exists? dbpath)))
		      (if fe 
			  fe
			  (begin
			    (create-directory (conc *toppath* "/db") #t)
			    #f))))
	 (sdb        (sqlite3:open-database dbpath))
	 (handler   (make-busy-timeout (if (args:get-arg "-override-timeout")
					   (string->number (args:get-arg "-override-timeout"))
					   136000))))
    (sqlite3:set-busy-handler! sdb handler)
    (if (not dbexists)
	(sdb:initialize sdb))
    (sqlite3:execute sdb "PRAGMA synchronous = 1;")
    sdb))

(define (sdb:initialize sdb)
  (sqlite3:execute sdb "CREATE TABLE IF NOT EXISTS strs
                           (id  INTEGER PRIMARY KEY,
                            str TEXT,
                        CONSTRAINT str UNIQUE (str));")
  (sqlite3:execute sdb "CREATE INDEX strindx ON strs (str);"))

;; (define sumup (let ((a 0))(lambda (x)(set! a (+ x a)) a)))

(define (sdb:register-string sdb str)
  (sqlite3:execute sdb "INSERT OR IGNORE INTO strs (str) VALUES (?);" str))

(define (sdb:string->id sdb str-cache str)
  (let ((id (hash-table-ref/default str-cache str #f)))
    (if (not id)
	(sqlite3:for-each-row
	 (lambda (sid)
	   (set! id sid)
	   (hash-table-set! str-cache str id))
	 sdb
	 "SELECT id FROM strs WHERE str=?;" str))
    id))

(define (sdb:id->string sdb id-cache id)
  (let ((str (hash-table-ref/default id-cache id #f)))
    (if (not str)
	(sqlite3:for-each-row
	 (lambda (istr)
	   (set! str istr)
	   (hash-table-set! id-cache id str))
	 sdb
	 "SELECT str FROM strs WHERE id=?;" id))
    str))

;; Numbers get passed though in both directions
;;
(define (make-sdb:qry #!key (fname #f))
  (let ((sdb    (sdb:open fname: fname))
	(scache (make-hash-table))
	(icache (make-hash-table)))
    (lambda (cmd var)
      ;; (if (not sdb)(set! sdb (sdb:open)))
      (case cmd
	;; ((init)      (if (not sdb)(set! sdb (sdb:open))))
	((finalize!) (if sdb (sqlite3:finalize! sdb)))
	((getid)     (let ((id (if (or (number? var)
				       (string->number var))
				   var
				   (sdb:string->id sdb scache var))))
		       (if id
			   id
			   (begin
			     (sdb:register-string sdb var)
			     (sdb:string->id sdb scache var)))))
	((getstr)    (if (or (number? var)
			     (string->number var))
			 (sdb:id->string sdb icache var)
			 var))
	(else #f)))))

