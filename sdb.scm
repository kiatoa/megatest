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
(include "/nfs/site/disks/icf_fdk_cw_gwa002/srehman/fossil/dbi/dbi.scm")
(import (prefix dbi dbi:))

(declare (unit sdb))

;; 
(define (sdb:open fname)
  (let* ((dbpath    (pathname-directory fname))
	 (dbexists  (let ((fe (file-exists? fname)))
		      (if fe 
			  fe
			  (begin
			    (create-directory dbpath #t)
			    #f))))
	 (sdb        (dbi:open 'sqlite3 (cons (cons ('dbname fname) '()))))
	 (handler   (make-busy-timeout 136000)))
    ;;(sqlite3:set-busy-handler! sdb handler)
    (if (not dbexists)
	(sdb:initialize sdb))
    (dbi:exec sdb "PRAGMA synchronous = 1;")
    sdb))

(define (sdb:initialize sdb)
  (dbi:exec sdb "CREATE TABLE IF NOT EXISTS strs
                           (id  INTEGER PRIMARY KEY,
                            str TEXT,
                        CONSTRAINT str UNIQUE (str));")
  (dbi:exec sdb "CREATE INDEX IF NOT EXISTS strindx ON strs (str);"))

;; (define sumup (let ((a 0))(lambda (x)(set! a (+ x a)) a)))

(define (sdb:register-string sdb str)
  (dbi:exec sdb "INSERT OR IGNORE INTO strs (str) VALUES (?);" str))

(define (sdb:string->id sdb str-cache str)
  (let ((id (hash-table-ref/default str-cache str #f)))
    (if (not id)
	(dbi:for-each-row
	 (lambda (sid)
	   (set! id (vector-ref sid 0))
	   (hash-table-set! str-cache str id))
	 sdb
	 "SELECT id FROM strs WHERE str=?;" str))
    id))

(define (sdb:id->string sdb id-cache id)
  (let ((str (hash-table-ref/default id-cache id #f)))
    (if (not str)
	(dbi:for-each-row
	 (lambda (istr)
	   (set! str (vector-ref istr 0))
	   (hash-table-set! id-cache id str))
	 sdb
	 "SELECT str FROM strs WHERE id=?;" id))
    str))

;; Numbers get passed though in both directions
;;
(define (make-sdb:qry fname)
  (let ((sdb    #f)
	(scache (make-hash-table))
	(icache (make-hash-table)))
    (lambda (cmd var)
      (case cmd
	((setup)   (set! sdb (if (not sdb)
				 (sdb:open (if var var fname)))))
	((setdb)    (set! sdb var))
	((getdb)    sdb)
	((finalize) (if sdb
			(begin
			  (dbi:close sdb)
			  (set! sdb #f))))
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
	((passid)    var)
	((passstr)   var)
	(else #f)))))

