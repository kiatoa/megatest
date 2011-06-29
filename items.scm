
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.


;; (define itemdat '((ripeness    "green ripe overripe")
;; 		     (temperature "cool medium hot")
;; 		     (season      "summer winter fall spring")))

;; Mostly worked = puts out all combinations?
(define (process-itemlist-try1 curritemkey itemlist)
  (let loop ((hed (car itemlist))
	     (tal (cdr itemlist)))
    (if (null? tal)
	(for-each (lambda (item)
		    (debug:print 6 "curritemkey: " (append curritemkey (list item))))
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

;; (item-assoc->item-list '(("ANIMAL" "Elephant Lion")("SEASON" "Spring Fall")))
;;   => ((("ANIMAL" "Elephant") ("SEASON" "Spring")) 
;;       (("ANIMAL" "Elephant") ("SEASON" "Fall")) 
;;       (("ANIMAL" "Lion")     ("SEASON" "Spring"))
;;       (("ANIMAL" "Lion")     ("SEASON" "Fall")))
(define (item-assoc->item-list itemsdat)
  (if (and itemsdat (not (null? itemsdat)))
      (let ((itemlst (filter (lambda (x)
			       (list? x))
			     (map (lambda (x)
				    (debug:print 6 "item-assoc->item-list x: " x)
				    (if (< (length x) 2)
					(begin
					  (debug:print 0 "ERROR: malformed items spec " (string-intersperse x " "))
					  (list (car x)'()))
					(let ((name (car x))
					      (items (cadr x)))
					  (list name (string-split items)))))
				  itemsdat))))
	(let ((debuglevel 5))
	  (debug:print 5 "item-assoc->item-list: itemsdat => itemlst ")
	  (if (>= *verbosity* 5)
	      (begin
		(pp itemsdat)
		(print " => ")
		(pp itemlst))))
	(if (> (length itemlst) 0)
	    (process-itemlist #f '() itemlst)
	    '()))
      '())) ;; return a list consisting on a single null list for non-item runs
            ;; Nope, not now, return null as of 6/6/2011

;; (item-table->item-list '(("ANIMAL" "Elephant Lion")("SEASON" "Spring Winter")))
;;   => ((("ANIMAL" "Elephant")("SEASON" "Spring"))
;;       (("ANIMAL" "Lion")    ("SEASON" "Winter")))
(define (item-table->item-list itemtable)
  (let ((newlst (map (lambda (x)
		       (if (> (length x) 1)
			   (list (car x)
				 (string-split (cadr x)))
			   (list x '())))
		     itemtable))
	(res     '())) ;; a list of items
    (let loop ((indx    0)
	       (item   '()) ;; an item will be ((KEYNAME1 VAL1)(KEYNAME2 VAL2) ...)
	       (elflag  #f))
      (for-each (lambda (row)
		  (let ((rowname (car row))
			(rowdat  (cadr row)))
		    (set! item (append item 
				       (list 
					(if (< indx (length rowdat))
					    (let ((new (list rowname (list-ref rowdat indx))))
					      ;; (debug:print 0 "New: " new)
					      (set! elflag #t)
					      new
					      ) ;; i.e. had at least on legit value to use
					    (list rowname "-")))))))
		newlst)
      (if elflag
	  (begin
	    (set! res (append res (list item)))
	    (loop (+ indx 1)
		  '()
		  #f)))
      res)))
            ;; Nope, not now, return null as of 6/6/2011
		
  
(define-inline (item-list->path itemdat)
  (string-intersperse  (map cadr itemdat) "/"))

;; (pp (item-assoc->item-list itemdat))


	
