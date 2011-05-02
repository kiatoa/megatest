
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.


;; (define itemdat '((ripeness    "green ripe overripe")
;; 		  (temperature "cool medium hot")
;; 		  (season      "summer winter fall spring")))

;; Mostly worked = puts out all combinations?
(define (process-itemlist-try1 curritemkey itemlist)
  (let loop ((hed (car itemlist))
	     (tal (cdr itemlist)))
    (if (null? tal)
	(for-each (lambda (item)
		    (print "curritemkey: " (append curritemkey (list item))))
		  (cadr hed))
	(begin
	  (for-each (lambda (item)
		      (process-itemlist (append curritemkey (list item)) tal))
		    (cadr hed))
	  (loop (car tal)(cdr tal))))))

;; Mostly worked = puts out all combinations?
(define (process-itemlist hierdepth curritemkey itemlist)
  (let ((res '()))
    (if (not hierdepth)
	(set! hierdepth (length itemlist)))
    (let loop ((hed (car itemlist))
	       (tal (cdr itemlist)))
      (if (null? tal)
	  (for-each (lambda (item)
		      (if (> (length curritemkey) (- hierdepth 2))
			  (set! res (append res (list (append curritemkey (list (list (car hed) item))))))))
		    (cadr hed))
	  (begin
	    (for-each (lambda (item)
			(set! res (append res (process-itemlist hierdepth (append curritemkey (list (list (car hed) item))) tal))))
		      (cadr hed))
	    (loop (car tal)(cdr tal)))))
    res))

(define (item-assoc->item-list itemsdat)
  (if (and itemsdat (not (null? itemsdat)))
      (let ((itemlst (map (lambda (x)
			    (let ((name (car x))
				  (items (cadr x)))
			      (list name (string-split items))))
			  itemsdat)))
	(process-itemlist #f '() itemlst))
      '(()))) ;; return a list consisting on a single null list for non-item runs
  
(define-inline (item-list->path itemdat)
  (string-intersperse  (map cadr itemdat) "/"))

;; (pp (item-assoc->item-list itemdat))


	
