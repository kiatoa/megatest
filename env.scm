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

(declare (unit env))

(use sql-de-lite) ;; srfi-1 posix regex regex-case srfi-69 nanomsg srfi-18 call-with-environment-variables)

(define (env:open-db fname)
  (let* ((db-exists (file-exists? fname))
	 (db        (open-database fname)))
    (if (not db-exists)
	(begin
	  (exec (sql db "CREATE TABLE envvars (
                    id INTEGER PRIMARY KEY,
                    context TEXT NOT NULL,
                    var TEXT NOT NULL,
                    val TEXT NOT NULL,
                       CONSTRAINT envvars_constraint UNIQUE (context,var))"))))
    (set-busy-handler! db (busy-timeout 10000))
    db))

;; save vars in given context, this is NOT incremental by default
;;
(define (env:save-env-vars db context #!key (incremental #f))
  (with-transaction
   db
   (lambda ()
     ;; first clear out any vars for this context
     (if (not incremental)(exec (sql db "DELETE FROM envvars WHERE context=?") context))
     (for-each
      (lambda (varval)
	(let ((var (car varval))
	      (val (cdr varval)))
	  (if incremental (exec (sql db "DELETE FROM envvars WHERE context=? AND var=?") context var))
	  (exec (sql db "INSERT INTO envvars (context,var,val) VALUES (?,?,?)") context var val)))
	(get-environment-variables)))))

;; apply contexts to current environment
;;  - each context is applied in the given order
;;  - variables in the paths list are split on the separator and the components
;;    merged using simple delta addition
;;
(define (env:apply-contexts db basecontext contexts paths outputf formats)
  
  (for-each
   (lambda (context)
     (query
      (for-each-row
       (lambda (row)
	 (let ((var  (car row))
	       (vala (cadr row))
	       (valb (caddr row)))
	    ;;(print "var: " var " vala: " vala " valb" valb " paths: " paths)
	   (if (assoc var paths) ;; this var is a PATH
	       (let ((current (get-environment-variable var))) ;; use this NOT vala
	         ;;(pp paths)
                 ;;(pp var)
		 (env:process-path-envvar var (cadr (assoc var paths)) current valb))
	       (begin
		 (setenv var valb))))))
      (sql db "SELECT b.var,a.val,b.val FROM envvars AS a JOIN envvars AS b ON a.var=b.var WHERE a.context=? AND b.context=? AND a.val != b.val")
      ;;(sql db "SELECT b.var,a.val,b.val FROM envvars AS a JOIN envvars AS b ON a.var=b.var WHERE a.context=? AND b.context=?")
      basecontext context))
   contexts))

(define (env:blind-merge l1 l2)
  (if (null? l1) l2
      (if (null? l2) l1
	  (cons (car l1) (cons (car l2) (env:blind-merge (cdr l1) (cdr l2)))))))

;; given a before and an after envvar calculate a new merged path
;;
(define (env:merge-path-envvar separator patha pathb)
  (let* ((patha-parts  (string-split patha separator))
	 (pathb-parts  (string-split pathb separator))
	 (common-parts (lset-intersection equal? patha-parts pathb-parts))
	 (final        (delete-duplicates ;; env:blind-merge 
			(append pathb-parts common-parts patha-parts))))
;;     (print "BEFORE:   " (string-intersperse patha-parts  "\n       "))
;;     (print "AFTER:    " (string-intersperse pathb-parts  "\n       "))
;;     (print "COMMON:   " (string-intersperse common-parts "\n       "))
    (string-intersperse final separator)))

(define (env:process-path-envvar varname separator patha pathb)
  (begin
    (print "Process-path-envvar: " varname)
  ) 
  (let ((newpath (env:merge-path-envvar separator patha pathb)))
    (setenv varname newpath)))

(define (env:have-context db context)
  (> (query fetch-value (sql db "SELECT count(id) FROM envvars WHERE context=?") context)
     0))

;; this is so the calling block does not need to import sql-de-lite
(define (env:close-database db)
  (close-database db))