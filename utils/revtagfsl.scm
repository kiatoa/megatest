
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

(define tagged   (make-hash-table))
(define usedtags (make-hash-table))

(for-each (lambda (node)
	    (let* ((uuid      (cdr (assoc "uuid" node)))
		   (tags-dat  (assoc "tags" node))
		   (tags      (if tags-dat (cdr tags-dat) '())))
	      (for-each (lambda (tag)
			  (hash-table-set! usedtags tag #t))
			tags)))
	  tl)

(define ord-tl (sort tl (lambda (a b)(let ((ta (cdr (assoc "timestamp" a)))(tb (cdr (assoc "timestamp" b))))(< ta tb)))))

(define (make-tag branch)
  (let* ((nextnum (+ 1 (hash-table-ref/default tagged branch 0))))
    (hash-table-set! tagged branch nextnum)
    (conc branch "-r" nextnum)))

(define (get-next-revtag branch)
  (let loop ((tag (make-tag branch)))
    (if (hash-table-ref/default usedtags tag #f)
	(loop (make-tag branch))
	tag)))

(print "branch, uuid, newtag")

(let loop ((hed (car ord-tl))
	   (tal (cdr ord-tl)))
  (let* ((tags    (let ((t (assoc "tags" hed)))
		    (if t (cdr t) '())))
	 (uuid    (cdr (assoc "uuid" hed)))
	 (branch  (if (null? tags) "nobranch" (car tags)))
	 (tagpatt (regexp (conc "^" branch "-r\\d+$")))
	 (currtag (filter (lambda (x)(string-match tagpatt x)) tags)))
    (if (and (not (equal? branch "nobranch"))
	     (null? currtag))
	(let ((newtag (get-next-revtag branch)))
	  (print branch ", " uuid ", " newtag)
	  (system (conc "fossil tag add \"" newtag "\" " uuid " -R " fname)) ;; ?--raw? ?--propagate? TAGNAME CHECK-IN ?VALUE?
	  (hash-table-set! usedtags currtag #t)))
    (if (not (null? tal))
	(loop (car tal)(cdr tal)))))
