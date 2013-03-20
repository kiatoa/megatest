
(use iup)

(define t #f) 

(define tree-dialog
  (dialog
   #:title "Tree Test"
   (let ((t1 (treebox
	      #:selection_cb (lambda (obj id state)
			       (print "selection_db with id=" id " state=" state)
			       (print "SPECIALDATA: " (attribute obj "SPECIALDATA"))
			       ))))
     (set! t t1)
     t1)))

(show tree-dialog)

(map (lambda (elname el)
       (print "Adding " elname " with value " el)
       (attribute-set! t elname el)
       (attribute-set! t "SPECIALDATA" el))
     '("VALUE" "NAME"    "ADDLEAF" "ADDBRANCH1" "ADDLEAF2"    "VALUE")
     '("0"     "Figures" "Other"   "triangle"   "equilateral" "4")
     )
(map (lambda (attr)
       (print attr " is " (attribute t attr)))
     '("KIND1" "PARENT2" "STATE1"))

(define (tree-find-node obj path)
  ;; start at the base of the tree
  (let loop ((hed     (car path))
	     (tal     (cdr path))
	     (depth   0)
	     (nodenum 0))
    (attribute-set! obj "VALUE" nodenum)
    (if (not (equal? (string->number (attribute obj "VALUE")) nodenum))
	;; when not equal we have reached the end of the line
	#f
	(let ((node-depth (string->number (attribute obj (conc "DEPTH" nodenum))))
	      (node-title (attribute obj (conc "TITLE" nodenum))))
	  (if (and (equal? depth node-depth)
		   (equal? hed   node-title)) ;; yep, this is the one!
	      (if (null? tal) ;; end of the line
		  nodenum
		  (loop (car tal)(cdr tal)(+ depth 1) nodenum))
	      (loop hed tal depth (+ nodenum 1)))))))

(main-loop)
