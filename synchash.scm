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
    (list changed deleted)
    ;; (list indat '()) ;; just for debugging
    ))
    
;; (cdb:remote-run db:get-keys #f)  
;; (cdb:remote-run db:get-num-runs #f "%")
;; (cdb:remote-run db:get-runs #f runnamepatt numruns *start-run-offset* keypatts)
;;
;; keynum => the field to use as the unique key (usually 0 but can be other field)
;;
(define (synchash:client-get proc synckey keynum synchash . params)
  (let* ((data   (apply cdb:remote-run synchash:server-get #f proc synckey keynum params))
	 (newdat (car data))
	 (removs (cadr data))
	 (myhash (hash-table-ref/default synchash synckey #f)))
    (if (not myhash)
	(begin
	  (set! myhash (make-hash-table))
	  (hash-table-set! synchash synckey myhash)))
    (for-each 
     (lambda (item)
       (let ((id  (car item))
	     (dat (cadr item)))
	 ;; (debug:print-info 2 "Processing item: " item)
	 (hash-table-set! myhash id dat)))
     newdat)
    (for-each
     (lambda (id)
       (hash-table-delete! myhash id))
     removs)
    (list newdat removs))) ;; synchash))


(define *synchashes* (make-hash-table))

(define (synchash:server-get db proc synckey keynum . params)
  ;; (debug:print-info 2 "synckey: " synckey ", keynum: " keynum ", params: " params)
  (let* ((synchash (hash-table-ref/default *synchashes* synckey #f))
	 (newdat   (apply (case proc
			    ((db:get-runs) db:get-runs)
			    ((db:get-tests-for-runs) db:get-tests-for-runs)
			    (else print))
			  db params))
	 (postdat  #f)
	 (make-indexed (lambda (x)
			 (list (vector-ref x keynum) x))))
    ;; Now process newdat based on the query type
    (set! postdat (case proc
		    ((db:get-runs)
		     ;; (debug:print-info 2 "Get runs call")
		     (let ((header (vector-ref newdat 0))
			   (data   (vector-ref newdat 1)))
		       ;; (debug:print-info 2 "header: " header ", data: " data)
		       (cons (list "header" header)         ;; add the header keyed by the word "header"
			     (map make-indexed data))))        ;; add each element keyed by the keynum'th val
		    (else 
		     ;; (debug:print-info 2 "Non-get runs call")
		     (map make-indexed newdat))))
    ;; (debug:print-info 2 "postdat: " postdat)
    (if (not synchash)
	(begin
	  (set! synchash (make-hash-table))
	  (hash-table-set! *synchashes* synckey synchash)))
    (synchash:get-delta postdat synchash)))

