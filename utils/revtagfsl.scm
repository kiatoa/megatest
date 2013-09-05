
;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(use json regex posix)
(use srfi-69)

;; Add tags with node nums: trunk(12) 
(define fname #f)

(let ((parms (argv)))
  (if (> (length parms) 1)
      (set! fname (cadr parms))))

(if (not (and fname (file-exists? fname)))
    (begin
      (print "Usage: revtagfsl /path/to/fossilfile.fossil")
      (exit 1)))

(define (revtag:get-timeline fslfname limit)
  (let* ((cmd      (if (file-exists? fslfname)
		       (conc "fossil json timeline checkin --limit " limit " -R " fslfname)
		       (conc "fossil json timeline checkin --limit " limit))))
    (with-input-from-pipe cmd json-read)))
    

(define mt (vector->list (revtag:get-timeline fname 10000)))
(define tl (map vector->list (cdr (assoc "timeline" (vector->list (cdr (assoc "payload" mt)))))))

(define nodes    (make-hash-table)) ;; look up for the nodes
(define parents  (make-hash-table)) ;; node-uuid -> (list parent ...)
(define children (make-hash-table)) ;; node-uuid -> (list child ...)
(define tagged   (make-hash-table))
(define usedtags (make-hash-table))

(define noparents '())

(for-each (lambda (node)
	    (let ((uuid      (cdr (assoc "uuid" node)))
		  (myparents (assoc "parents" node)))
	      (hash-table-set! nodes uuid node)
	      (if myparents
		  (begin
		    (hash-table-set! parents uuid (cdr myparents))
		    (for-each (lambda (parent)
				(hash-table-set! children parent (cons uuid (hash-table-ref/default children parent '()))))
			      myparents))
		  (set! noparents (cons node noparents)))))
	  tl)

(define ord-tl (sort tl (lambda (a b)(let ((ta (cdr (assoc "timestamp" a)))(tb (cdr (assoc "timestamp" b))))(< ta tb)))))

(print "branch, uuid, newtag")
(let loop ((hed (car ord-tl))
	   (tal (cdr ord-tl)))
  (let* ((tags    (let ((t (assoc "tags" hed)))
		    (if t (cdr t) '())))
	 (uuid    (cdr (assoc "uuid" hed)))
	 (branch  (if (null? tags) "nobranch" (car tags)))
	 (nextnum (+ 1 (hash-table-ref/default tagged branch 0)))
	 (tagpatt (regexp (conc "^" branch "\\(\\d+\\)")))
	 (currtag (filter (lambda (x)(string-match tagpatt x)) tags))
	 (newtag  (conc branch "(" nextnum ")")))
    (if (and (not (equal? branch "nobranch"))
	     (null? currtag))
	(begin
	  (hash-table-set! tagged branch nextnum)
	  (print branch ", " uuid ", " newtag)
	  (system (conc "fossil tag add \"" newtag "\" " uuid " -R " fname)) ;; ?--raw? ?--propagate? TAGNAME CHECK-IN ?VALUE?
	  (hash-table-set! usedtags currtag #t))
	(for-each (lambda (t)
		    (hash-table-set! usedtags t #t))
		  currtag))
    (if (not (null? tal))
	(loop (car tal)(cdr tal)))))
    
	 