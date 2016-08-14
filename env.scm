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
(define (env:save-env-vars db context #!key (incremental #f)(vardat #f))
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
	(if vardat
	    (hash-table->alist vardat)
	    (get-environment-variables))))))

;; merge contexts in the order given
;;  - each context is applied in the given order
;;  - variables in the paths list are split on the separator and the components
;;    merged using simple delta addition
;;    returns a hash of the merged vars
;;
(define (env:merge-contexts db basecontext contexts paths)
  (let ((result (make-hash-table)))
    (for-each
     (lambda (context)
       (query
	(for-each-row
	 (lambda (row)
	   (let ((var  (car row))
		 (val  (cadr row)))
	     (hash-table-set! result var 
			      (if (and (hash-table-ref/default results var #f)
				       (assoc var paths)) ;; this var is a path and there is a previous path
				  (let ((sep (cadr (assoc var paths))))
				    (env:merge-path-envvar sep (hash-table-ref results var) valb))
				  valb)))))
	(sql db "SELECT var,val FROM envvars WHERE context=?")
	context))
     contexts)
    result))

;;  get list of removed variables between two contexts
;;
(define (env:get-removed db contexta contextb)
  (let ((result (make-hash-table)))
    (query
     (for-each-row
      (lambda (row)
	(let ((var  (car row))
	      (val  (cadr row)))
	  (hash-table-set! result var val))))
     (sql db "SELECT var,val FROM envvars WHERE context=? AND var NOT IN (SELECT var FROM envvars WHERE context=?)")
     contexta contextb)
    result))

;;  get list of variables added to contextb from contexta
;;
(define (env:get-added db contexta contextb)
  (let ((result (make-hash-table)))
    (query
     (for-each-row
      (lambda (row)
	(let ((var  (car row))
	      (val  (cadr row)))
	  (hash-table-set! result var val))))
     (sql db "SELECT var,val FROM envvars WHERE context=? AND var NOT IN (SELECT var FROM envvars WHERE context=?)")
     contextb contexta)
    result))

;;  get list of variables in both contexta and contexb that have been changed
;;
(define (env:get-changed db contexta contextb)
  (let ((result (make-hash-table)))
    (query
     (for-each-row
      (lambda (row)
	(let ((var  (car row))
	      (val  (cadr row)))
	  (hash-table-set! result var val))))
     (sql db "SELECT var,val FROM envvars AS a WHERE context=? AND val != (SELECT val FROM envvars WHERE var=a.var AND context=?)")
     contexta contextb)
    result))

;;
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
  (let ((newpath (env:merge-path-envvar separator patha pathb)))
    (setenv varname newpath)))

(define (env:have-context db context)
  (> (query fetch-value (sql db "SELECT count(id) FROM envvars WHERE context=?") context)
     0))

;; this is so the calling block does not need to import sql-de-lite
(define (env:close-database db)
  (close-database db))

(define (env:lazy-hash-table->alist indat)
  (if (hash-table? indat)
      (let ((dat (hash-table->alist indat)))
	(if (null? dat)
	    #f 
	    dat))
      #f))

(define (env:inc-path path)
  (print "PATH "
	 (conc "#{scheme (env:min-path \"" path "\" \"#{getenv PATH}\")}")))
;; 	 (conc
;; 	  "#{scheme (string-intersperse "
;; 	  "(delete-duplicates "
;; 	  "(append (string-split \"" path "\" \":\") "
;; 	  "(string-split \"#{getenv PATH}\" \":\")))"
;; 	  " \":\")}")))

(define (env:min-path path1 path2)
  (string-intersperse
   (delete-duplicates
    (append
     (string-split path1 ":")
     (string-split path2 ":")))
   ":"))

;; inc path will set a PATH that is incrementally modified when read - config mode only
;;
(define (env:print added removed changed #!key (inc-path #t))
  (let ((a  (env:lazy-hash-table->alist added))
	(r  (env:lazy-hash-table->alist removed))
	(c  (env:lazy-hash-table->alist changed)))
    (case (if (args:get-arg "-dumpmode")
	      (string->symbol (args:get-arg "-dumpmode"))
	      'bash)
      ((bash)
       (if a
	   (begin
	     (print "# Added vars")
	     (map (lambda (dat)(print "export " (car dat) "=" (cdr dat)))
		  (hash-table->alist added))))
       (if r
	   (begin
	     (print "# Removed vars")
	     (map (lambda (dat)(print "unset " (car dat)))
		  (hash-table->alist removed))))
       (if c
	   (begin
	     (print "# Changed vars")
	     (map (lambda (dat)(print "export " (car dat) "=" (cdr dat)))
		  (hash-table->alist changed)))))
      ((csh)
       (if a
	   (begin
	     (print "# Added vars")
	     (map (lambda (dat)(print "setenv " (car dat) " " (cdr dat)))
		  (hash-table->alist added))))
       (if r
	   (begin
	     (print "# Removed vars")
	     (map (lambda (dat)(print "unsetenv " (car dat)))
		  (hash-table->alist removed))))
       (if c
	   (begin
	     (print "# Changed vars")
	     (map (lambda (dat)(print "setenv " (car dat) " " (cdr dat)))
		  (hash-table->alist changed)))))
      ((config ini)
       (if a
	   (begin
	     (print "# Added vars")
	     (map (lambda (dat)
		    (let ((var (car dat))
			  (val (cdr dat)))
		      (if (and inc-path
			       (equal? var "PATH"))
			  (env:inc-path val)
			  (print var " " val))))
		  (hash-table->alist added))))
       (if r
	   (begin
	     (print "# Removed vars")
	     (map (lambda (dat)(print "#{scheme (unsetenv \"" (car dat) "\")}"))
		  (hash-table->alist removed))))
       (if c
	   (begin
	     (print "# Changed vars")
	     (map (lambda (dat)
		    (let ((var (car dat))
			  (val (cdr dat)))
		      (if (and inc-path
			       (equal? var "PATH"))
			  (env:inc-path val)
			  (print var " " val))))
		  (hash-table->alist changed)))))
      (else
       (debug:print-error 0 *default-log-port* "No dumpmode specified, use -dumpmode [bash|csh|config]")))))
