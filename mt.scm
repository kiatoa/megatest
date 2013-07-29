;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.


(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking (srfi 18) posix-extras directory-utils)
(import (prefix sqlite3 sqlite3:))

(declare (unit mt))
(declare (uses db))
(declare (uses common))
(declare (uses items))
(declare (uses runconfig))
(declare (uses tests))
(declare (uses server))
(declare (uses runs))

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "test_records.scm")

;; This is the Megatest API. All generally "useful" routines will be wrapped or extended
;; here.

;;======================================================================
;;  R U N S
;;======================================================================

;; runs:get-runs-by-patt
;; get runs by list of criteria
;; register a test run with the db
;;
;; Use: (db-get-value-by-header (db:get-header runinfo)(db:get-row runinfo))
;;  to extract info from the structure returned
;;
(define (mt:get-runs-by-patt keys runnamepatt targpatt)
  (let loop ((runsdat  (cdb:remote-run db:get-runs-by-patt #f keys runnamepatt targpatt 0 500))
	     (res      '())
	     (offset   0)
	     (limit    500))
    (print "runsdat: " runsdat)
    (let* ((header    (vector-ref runsdat 0))
	   (runslst   (vector-ref runsdat 1))
	   (full-list (append res runslst))
	   (have-more (eq? (length runslst) limit)))
      ;; (debug:print 0 "header: " header " runslst: " runslst " have-more: " have-more)
      (if have-more 
	  (let ((new-offset (+ offset limit))
		(next-batch (cdb:remote-run db:get-runs-by-patt #f keys runnamepatt targpatt offset limit)))
	    (debug:print-info 4 "More than " limit " runs, have " (length full-list) " runs so far.")
	    (debug:print-info 0 "next-batch: " next-batch)
	    (loop next-batch
		  full-list
		  new-offset
		  limit))
	 (vector header full-list)))))

;;======================================================================
;;  T E S T S
;;======================================================================

(define (mt:get-tests-for-run run-id testpatt states status #!key (not-in #t) (sort-by #f) (qryvals #f))
  (let loop ((testsdat (cdb:remote-run db:get-tests-for-run #f run-id testpatt states status 0 500 not-in sort-by qryvals: qryvals))
	     (res      '())
	     (offset   0)
	     (limit    500))
    (let* ((full-list (append res testsdat))
	   (have-more (eq? (length testsdat) limit)))
      (if have-more 
	  (let ((new-offset (+ offset limit)))
	    (debug:print-info 4 "More than " limit " tests, have " (length full-list) " tests so far.")
	    (loop (cdb:remote-run db:get-tests-for-run #f run-id testpatt states status new-offset limit not-in sort-by qryvals: qryvals)
		  full-list
		  new-offset
		  limit))
	  full-list))))

(define (mt:get-prereqs-not-met run-id waitons ref-item-path #!key (mode 'normal))
  (db:get-prereqs-not-met run-id waitons ref-item-path mode: mode))

(define (mt:get-run-stats)
  (cdb:remote-run db:get-run-stats #f))

;;======================================================================
;;  S T E P S
;;======================================================================

