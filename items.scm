
;; Copyright 2006-2012, Matthew Welland.
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

(declare (unit items))
(declare (uses common))

(include "common_records.scm")

;; Puts out all combinations
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
					(let* ((name (car x))
					       (items (cadr x))
					       (ilist (list name (if (string? items)
								     (string-split items)
								     '()))))
					  (if (null? ilist)
					      (debug:print 0 "ERROR: No items specified for " name))
					  ilist)))
				  itemsdat))))
	(let ((debuglevel 5))
	  (debug:print 5 "item-assoc->item-list: itemsdat => itemlst ")
	  (if (debug:debug-mode 5)
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
		
(define (items:check-valid-items class item)
  (let ((valid-values (let ((s (config-lookup *configdat* "validvalues" class)))
			(if s (string-split s) #f))))
    (if valid-values
	(if (member item valid-values)
	    item #f)
	item)))

(define (items:get-items-from-config tconfig)
  (let* ((have-items  (hash-table-ref/default tconfig "items"      #f))
	 (have-itable (hash-table-ref/default tconfig "itemstable" #f))
	 (items       (hash-table-ref/default tconfig "items"      '()))
	 (itemstable  (hash-table-ref/default tconfig "itemstable" '())))
    (debug:print 5 "items: " items " itemstable: " itemstable)
    (set! items (map (lambda (item)
		       (if (procedure? (cadr item))
			   (list (car item)((cadr item)))
			   item))
		     items))
    (set! itemstable (map (lambda (item)
			    (if (procedure? (cadr item))
				(list (car item)((cadr item)))
				item))
			  itemstable))
    (if (and have-items  (null? items))     (debug:print 0 "ERROR: [items] section in testconfig but no entries defined"))
    (if (and have-itable (null? itemstable))(debug:print 0 "ERROR: [itemstable] section in testconfig but no entries defined"))
    (if (or (not (null? items))(not (null? itemstable)))
	(append (item-assoc->item-list items)
		(item-table->item-list itemstable))
	'(()))))

;; (pp (item-assoc->item-list itemdat))


	
