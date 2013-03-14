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

;;======================================================================
;; A hash of hashes that can be kept in sync by sending minial deltas
;;======================================================================

(use format)
(use srfi-1 srfi-69)

(declare (unit synchash))
(declare (uses db))
(declare (uses server))
(include "db_records.scm")

(define (synchash:make)
   (make-hash-table))

;; given an alist of objects '((id obj) ...) 
;;   1. remove unchanged objects from the list
;;   2. create a list of removed objects by id
;;   3. remove removed objects from synchash
;;   4. replace or add new or changed objects to synchash
;;
(define (synchash:get-delta indat synchash)
  (let ((deleted '())
	(changed '())
	(found   '())
	(orig-keys (hash-table-keys synchash)))
    (for-each
     (lambda (item)
       (let* ((id  (car  item))
	      (dat (cadr item))
	      (ref (hash-table-ref/default synchash id #f)))
	 (if (not (equal? dat ref)) ;; item changed or new
	     (begin
	       (set! changed (cons item changed))
	       (hash-table-set! synchash id dat)))
	 (set! found (cons id found))))
     indat)
    (for-each 
     (lambda (id)
       (if (not (member id found))
	   (begin
	     (set! deleted (cons id deleted))
	     (hash-table-delete! synchash id))))
     orig-keys)
    (list changed deleted)))
    
;; (cdb:remote-run db:get-keys #f)  
;; (cdb:remote-run db:get-num-runs #f "%")
;; (cdb:remote-run db:get-runs #f runnamepatt numruns *start-run-offset* keypatts)
(define (synchash:client-get proc synckey keynum synchash . params)
  (let* ((data   (apply cdb:remote-run synchash:server-get #f proc synckey params))
	 (newdat (car data))
	 (removs (cadr data)))
    (for-each 
     (lambda (item)
       (let ((id  (car item))
	     (dat (cadr item)))
	 (hash-table-set! synchash id dat)))
     newdat)
    (for-each
     (lambda (id)
       (hash-table-delete! synchash id))
     removs)
    synchash))


(define *synchashes* (make-hash-table))

(define (synchash:server-get db proc synckey keynum . params)
  (let* ((synchash (hash-table-ref/default *synchashes* synckey #f))
	 (newdat   (apply proc db params)))
    (if (not synchash)
	(begin
	  (set! synchash (make-hash-table))
	  (hash-table-set! *synchashes* synckey synchash)))
    (synchash:get-delta newdat synchash)))

