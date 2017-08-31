
;; Copyright 2006-2012, Matthew Welland.
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

(define (keys->keystr keys) ;; => key1,key2,key3,additiona1, ...
  (string-intersperse keys ","))

(define (args:usage . a) #f)

;;======================================================================
;; key <=> target routines
;;======================================================================

;; This invalidates using "/" in item names. Every key will be
;; available via args:get-arg as :keyfield. Since this only needs to
;; be called once let's use it to set the environment vars
;;
;; The setting of :keyfield in args should be turned off ASAP
;;
(define (keys:target-set-args keys target ht)
  (if target
      (let ((vals (string-split target "/")))
	(if (eq? (length vals)(length keys))
	    (for-each (lambda (key val)
			(setenv key val)
			(if ht (hash-table-set! ht (conc ":" key) val)))
		      keys
		      vals)
	    (debug:print-error 0 *default-log-port* "wrong number of values in " target ", should match " keys))
	vals)
      (debug:print 4 *default-log-port* "ERROR: keys:target-set-args called with no target.")))

;; given the keys (a list of vectors <key field> or a list of keys) and a target return a keyval list
;; keyval list ( (key1 val1) (key2 val2) ...)
(define (keys:target->keyval keys target)
  (let* ((targlist (string-split target "/"))
	 (numkeys  (length keys))
	 (numtarg  (length targlist))
	 (targtweaked (if (> numkeys numtarg)
			  (append targlist (make-list (- numkeys numtarg) ""))
			  targlist)))
    (map (lambda (key targ)
	   (list key targ))
	 keys targtweaked)))

;;======================================================================
;; config file related routines
;;======================================================================

(define (keys:config-get-fields confdat)
  (let ((fields (hash-table-ref/default confdat "fields" '())))
    (map car fields)))

(define (keys:make-key/field-string confdat)
  (let ((fields (configf:get-section confdat "fields")))
    (string-join
     (map (lambda (field)(conc (car field) " " (cadr field)))
	  fields)
     ",")))

