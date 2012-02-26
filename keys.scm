
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

(use sqlite3 srfi-1 posix regex regex-case srfi-69)
(import (prefix sqlite3 sqlite3:))

(declare (unit keys))
(declare (uses common))

(include "key_records.scm")
(include "common_records.scm")

(define (get-keys db)
  (let ((keys '())) ;; keys are vectors <fieldname,type>
    (sqlite3:for-each-row (lambda (fieldname fieldtype)
			    (set! keys (cons (vector fieldname fieldtype) keys)))
			  db
			  "SELECT fieldname,fieldtype FROM keys ORDER BY id ASC;")
    (reverse keys))) ;; could just sort desc?

(define (keys->keystr keys) ;; => key1,key2,key3,additiona1, ...
  (string-intersperse (map key:get-fieldname keys) ","))

(define (args:usage . a) #f)

;; keys->vallist is called several times (quite unnecessarily), use this hash to suppress multiple
;; reporting of missing keys on the command line.
(define keys:warning-suppress-hash (make-hash-table))

;;======================================================================
;; key <=> target routines
;;======================================================================

;; this now invalidates using "/" in item names
(define (keys:target-set-args keys target ht)
  (let ((vals (string-split target "/")))
    (if (eq? (length vals)(length keys))
	(for-each (lambda (key val)
		    (hash-table-set! ht (conc ":" (vector-ref key 0)) val))
		  keys
		  vals)
	(debug:print 0 "ERROR: wrong number of values in " target ", should match " keys))
    vals))

;; given the keys (a list of vectors <key field>) and a target return a keyval list
;; keyval list ( (key1 val1) (key2 val2) ...)
(define (keys:target->keyval keys target)
  (let* ((targlist (string-split target "/"))
	 (numkeys  (length keys))
	 (numtarg  (length targlist))
	 (targtweaked (if (> numkeys numtarg)
			  (append targlist (make-list (- numkeys numtarg) ""))
			  targlist)))
    (map (lambda (key targ)
	   (list (vector-ref key 0) targ))
	 keys targtweaked)))


;;======================================================================
;; key <=> args routines
;;======================================================================

;; Using the keys pulled from the database (initially set from the megatest.config file)
;; look for the equivalent value on the command line and add it to a list, or #f if not found.
;; default => (val1 val2 val3 ...)
;; withkey => (:key1 val1 :key2 val2 :key3 val3 ...)
(define (keys->vallist keys . withkey) ;; ORDERING IS VERY IMPORTANT, KEEP PROPER ORDER HERE!
  (let* ((keynames   (map key:get-fieldname keys))
	 (argkeys    (map (lambda (k)(conc ":" k)) keynames))
	 (withkey    (not (null? withkey)))
	 (newremargs (args:get-args 
		      (cons "blah" remargs) ;; the cons blah works around a bug in args [args assumes ("calling-prog-name" .... ) ]
		      argkeys 
		      '()
		      args:arg-hash 
		      0)))
    ;;(debug:print 0 "remargs: " remargs " newremargs: " newremargs)
    (apply append (map (lambda (x)
			 (let ((val (args:get-arg x)))
			   ;; (debug:print 0 "x: " x " val: " val)
			   (if (not val)
			       (begin
				 (if (not (hash-table-ref/default keys:warning-suppress-hash x #f))
				     (begin
				       (debug:print 0 "WARNING: missing key " x ". Specified in database but not on command line, using \"unk\"")
				       (hash-table-set! keys:warning-suppress-hash x #t)))
				 (set! val "default")))
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

