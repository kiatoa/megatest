
(use iup test)

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
  (if (null? path)
      #f ;; or 0 ????
      (let loop ((hed      (car path))
		 (tal      (cdr path))
		 (depth    0)
		 (nodenum  0))
	;; (debug:print 0 "hed: " hed ", depth: " depth ", nodenum: " nodenum)
	;; nodes in iup tree are 100% sequential so iterate over nodenum
	(if (attribute obj (conc "DEPTH" nodenum)) ;; end when no more nodes
	    (let ((node-depth (string->number (attribute obj (conc "DEPTH" nodenum))))
		  (node-title (attribute obj (conc "TITLE" nodenum))))
	      ;; (print 0 "hed: " hed ", depth: " depth ", node-depth: " node-depth ", nodenum: " nodenum ", node-title: " node-title)
	      (if (and (equal? depth node-depth)
		       (equal? hed   node-title)) ;; yep, this is the one!
		  (if (null? tal) ;; end of the line
		      nodenum
		      (loop (car tal)(cdr tal)(+ depth 1)(+ 1 nodenum)))
		  ;; this is the case where we found part of the hierarchy but not 
		  ;; all of it, i.e. the node-depth went from deep to less deep
		  (if (> depth node-depth) ;; (+ 1 node-depth))
		      #f
		      (loop hed tal depth (+ nodenum 1)))))
	    #f))))

;; top is the top node name zeroeth node VALUE=0
(define (tree-add-node obj top nodelst)
  (if (not (attribute obj "TITLE0"))
      (attribute-set! obj "ADDBRANCH0" top))
  (cond
   ((not (string=? top (attribute obj "TITLE0")))
    (print "ERROR: top name " top " doesn't match " (attribute obj "TITLE0")))
   ((null? nodelst))
   (else
    (let loop ((hed      (car nodelst))
	       (tal      (cdr nodelst))
	       (depth    1)
	       (pathl    (list top)))
      ;; Because the tree dialog changes node numbers when
      ;; nodes are added or removed we must look up nodes
      ;; each and every time. 0 is the top node so default
      ;; to that.
      (let* ((newpath    (append pathl (list hed)))
	       (parentnode (tree-find-node obj pathl))
	       (nodenum    (tree-find-node obj newpath)))
	  ;; (print "newpath: " newpath ", nodenum " nodenum ", hed: " hed ", depth: " depth ", parentnode: " parentnode ", pathl: " pathl)
	  ;; Add the branch under lastnode if not found
	  (if (not nodenum)
	      (begin
		(attribute-set! obj (conc "ADDBRANCH" parentnode) hed)
		(if (null? tal)
		    #t
		    ;; reset to top
		    (loop (car nodelst)(cdr nodelst) 1 (list top)))) 
	      (if (null? tal) ;; if null here then this path has already been added
		  #t
		  ;; (if nodenum
		  (loop (car tal)(cdr tal)(+ depth 1) newpath)))))))) ;;  (if nodenum nodenum lastnode)))))))
	      ;; 	  (loop hed tal depth pathl lastnode)))))))

(test #f 0  (tree-find-node t '("Figures")))
(test #f 1  (tree-find-node t '("Figures" "Other")))
(test #f #f (tree-find-node t '("Figures" "Other"    "equilateral")))
(test #f 3  (tree-find-node t '("Figures" "triangle" "equilateral")))
(test #f #t (tree-add-node  t "Figures" '()))
(test #f #t (tree-add-node  t "Figures" '("a" "b" "c")))
(test #f 3  (tree-find-node t '("Figures" "a" "b" "c")))
(test #f #t (tree-add-node  t "Figures" '("d" "b" "c")))
(test #f 3  (tree-find-node t '("Figures" "d" "b" "c")))
(test #f 6  (tree-find-node t '("Figures" "a" "b" "c")))
(test #f #t (tree-add-node  t "Figures" '("a" "e" "c")))
(test #f 6  (tree-find-node t '("Figures" "a" "e" "c")))
(main-loop)

