;;======================================================================
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

(include "db_records.scm")

(define-inline (runs:runrec-make-record) (make-vector 13))
(define-inline (runs:runrec-get-target  vec)(vector-ref vec 0))  ;; a/b/c
(define-inline (runs:runrec-get-runname vec)(vector-ref vec 1))  ;; string
(define-inline (runs:runrec-testpatt    vec)(vector-ref vec 2))  ;; a,b/c,d%
(define-inline (runs:runrec-keys        vec)(vector-ref vec 3))  ;; (key1 key2 ...)
(define-inline (runs:runrec-keyvals     vec)(vector-ref vec 4))  ;; ((key1 val1)(key2 val2) ...)
(define-inline (runs:runrec-environment vec)(vector-ref vec 5))  ;; environment, alist key val
(define-inline (runs:runrec-mconfig     vec)(vector-ref vec 6))  ;; megatest.config
(define-inline (runs:runrec-runconfig   vec)(vector-ref vec 7))  ;; runconfigs.config
(define-inline (runs:runrec-serverdat   vec)(vector-ref vec 8))  ;; (host port)
(define-inline (runs:runrec-transport   vec)(vector-ref vec 9))  ;; 'http
(define-inline (runs:runrec-db          vec)(vector-ref vec 10)) ;; <sqlite3db> (if 'fs)
(define-inline (runs:runrec-top-path    vec)(vector-ref vec 11)) ;; *toppath*
(define-inline (runs:runrec-run_id      vec)(vector-ref vec 12)) ;; run-id

(define-inline (test:get-id vec)       (db:test-rec-id vec))
(define-inline (test:get-run_id vec)   (db:test-rec-run_id vec))
(define-inline (test:get-test-name vec)(db:test-rec-testname vec))
(define-inline (test:get-state vec)    (db:test-rec-state vec))
(define-inline (test:get-status vec)   (db:test-rec-status vec))
(define-inline (test:get-item-path vec)(db:test-rec-item_path vec))

(define-inline (test:test-get-fullname test)
   (conc (db:test-get-testname test)
	 (if (equal? (db:test-get-item-path test) "")
	     ""
	     (conc "(" (db:test-get-item-path test) ")"))))

