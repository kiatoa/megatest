
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
 
;;======================================================================
;; Run keys, these are used to hierarchially organise tests and run areas
;;======================================================================


(define-inline (key:get-fieldname key)(vector-ref key 0))
(define-inline (key:get-fieldtype key)(vector-ref key 1))

(define (get-keys db)
  (let ((keys '())) ;; keys are vectors <fieldname,type>
    (sqlite3:for-each-row (lambda (fieldname fieldtype)
			    (set! keys (cons (vector fieldname fieldtype) keys)))
			  db
			  "SELECT fieldname,fieldtype FROM keys ORDER BY id ASC;")
    (reverse keys))) ;; could just sort desc?

;; get key vals for a given run-id
(define (get-key-vals db run-id)
  (let* ((keys (get-keys db))
	 (res  '()))
    (debug:print 6 "keys: " keys " run-id: " run-id)
    (for-each 
     (lambda (key)
       (let ((qry (conc "SELECT " (key:get-fieldname key) " FROM runs WHERE id=?;")))
	 ;; (debug:print 0 "qry: " qry)
	 (sqlite3:for-each-row 
	  (lambda (key-val)
	    (set! res (cons key-val res)))
	  db qry run-id)))
     keys)
    (reverse res)))

;; get key val pairs for a given run-id
;; ( (FIELDNAME1 keyval1) (FIELDNAME2 keyval2) ... )
(define (keys:get-key-val-pairs db run-id)
  (let* ((keys (get-keys db))
	 (res  '()))
    (debug:print 6 "keys: " keys " run-id: " run-id)
    (for-each 
     (lambda (key)
       (let ((qry (conc "SELECT " (key:get-fieldname key) " FROM runs WHERE id=?;")))
	 ;; (debug:print 0 "qry: " qry)
	 (sqlite3:for-each-row 
	  (lambda (key-val)
	    (set! res (cons (list (key:get-fieldname key) key-val) res)))
	  db qry run-id)))
     keys)
    (reverse res)))

(define (keys->keystr keys) ;; => key1,key2,key3,additiona1, ...
  (string-intersperse (map key:get-fieldname keys) ","))

(define-inline (keys->valslots keys) ;; => ?,?,? ....
  (string-intersperse (map (lambda (x) "?") keys) ","))

(define-inline (keys->key/field keys . additional)
  (string-join (map (lambda (k)(conc (key:get-fieldname k) " " (key:get-fieldtype k)))(append keys additional)) ","))

(define (args:usage . a) #f)

;; Using the keys pulled from the database (initially set from the megatest.config file)
;; look for the equivalent value on the command line and add it to a list, or #f if not found.
;; default => (val1 val2 val3 ...)
;; withkey => (:key1 val1 :key2 val2 :key3 val3 ...)
(define (keys->vallist keys . withkey) ;; ORDERING IS VERY IMPORTANT, KEEP PROPER ORDER HERE!
  (let* ((keynames   (map key:get-fieldname keys))
	 (argkeys    (map (lambda (k)(conc ":" k)) keynames))
	 (withkey    (not (null? withkey)))
	 (newremargs (args:get-args (cons "blah" remargs) argkeys '() args:arg-hash 0))) ;; the cons blah works around a bug in args [args assumes ("calling-prog-name" .... ) ]
    ;;(debug:print 0 "remargs: " remargs " newremargs: " newremargs)
    (apply append (map (lambda (x)
			 (let ((val (args:get-arg x)))
			   ;; (debug:print 0 "x: " x " val: " val)
			   (if (not val)
			       ;; (debug:print 0 "WARNING: missing key " x ". Specified in database but not on command line, using \"unk\"")
			       (set! val "default"))
			   (if withkey (list x val) (list val))))
		       argkeys))))
  
;; Given a list of keys (list of vectors) return an alist ((key argval) ...)
(define (keys->alist keys defaultval)
  (let* ((keynames   (map key:get-fieldname keys))
	 (newremargs (args:get-args (cons "blah" remargs) (map (lambda (k)(conc ":" k)) keynames) '() args:arg-hash 0))) ;; the cons blah works around a bug in args
    (map (lambda (key)
	   (let ((val (args:get-arg (conc ":" key))))
	     (list key (if val val defaultval))))
	 keynames)))

(define (keystring->keys keystring)
  (map (lambda (x)
	 (let ((xlst (string-split x ":")))
	   (list->vector (if (> (length xlst) 1) xlst (append (car xlst)(list "TEXT"))))))
       (delete-duplicates (string-split keystring ","))))

(define (config-get-fields confdat)
  (let ((fields (hash-table-ref/default confdat "fields" '())))
    (map (lambda (x)(vector (car x)(cadr x)))
	 fields)))

