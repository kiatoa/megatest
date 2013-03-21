
(use test)
(require-library iup)
(import (prefix iup iup:))

(define t #f) 

(define tree-dialog
  (iup:dialog
   #:title "Tree Test"
   (let ((t1 (iup:treebox
	      #:selection_cb (lambda (obj id state)
			       (print "selection_db with id=" id " state=" state)
			       (print "USERDATA: " (iup:attribute obj "USERDATA"))
			       (print "SPECIALDATA: " (iup:attribute obj "SPECIALDATA"))
			       (print "Depth: " (iup:attribute obj "DEPTH"))
			       ))))
     (set! t t1)
     t1)))

(iup:show tree-dialog)

(map (lambda (elname el)
       (print "Adding " elname " with value " el)
       (iup:attribute-set! t elname el)
       (iup:attribute-set! t "USERDATA" el))
     '("VALUE" "NAME"    "ADDLEAF" "ADDBRANCH1" "ADDLEAF2"    "VALUE")
     '("0"     "Figures" "Other"   "triangle"   "equilateral" "4")
     )
(map (lambda (attr)
       (print attr " is " (iup:attribute t attr)))
     '("KIND1" "PARENT2" "STATE1"))

(define (tree-find-node obj path)
  ;; start at the base of the tree
  (if (null? path)
      #f ;; or 0 ????
      (let loop ((hed      (car path))
		 (tal      (cdr path))
		 (depth    0)
		 (nodenum  0))
	;; nodes in iup tree are 100% sequential so iterate over nodenum
	(if (iup:attribute obj (conc "DEPTH" nodenum)) ;; end when no more nodes
	    (let ((node-depth (string->number (iup:attribute obj (conc "DEPTH" nodenum))))
		  (node-title (iup:attribute obj (conc "TITLE" nodenum))))
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
  (if (not (iup:attribute obj "TITLE0"))
      (iup:attribute-set! obj "ADDBRANCH0" top))
  (cond
   ((not (string=? top (iup:attribute obj "TITLE0")))
    (print "ERROR: top name " top " doesn't match " (iup:attribute obj "TITLE0")))
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
		(iup:attribute-set! obj (conc "ADDBRANCH" parentnode) hed)
		(if (null? tal)
		    #t
		    ;; reset to top
		    (loop (car nodelst)(cdr nodelst) 1 (list top)))) 
	      (if (null? tal) ;; if null here then this path has already been added
		  #t
		  ;; (if nodenum
		  (loop (car tal)(cdr tal)(+ depth 1) newpath)))))))) ;;  (if nodenum nodenum lastnode)))))))
	      ;; 	  (loop hed tal depth pathl lastnode)))))))

(define (tree-node->path obj nodenum)
  ;; (print "\ncurrnode  nodenum  depth  node-depth  node-title   path")
  (let loop ((currnode 0)
	     (depth    0)
	     (path     '()))
    (let ((node-depth (iup:attribute obj (conc "DEPTH" currnode)))
	  (node-title (iup:attribute obj (conc "TITLE" currnode))))
      ;; (display (conc "\n   "currnode "        " nodenum "       " depth "         " node-depth "          " node-title "         " path))
      (if (> currnode nodenum)
	  path
	  (if (not node-depth) ;; #f if we are out of nodes
	      '()
	      (let ((ndepth (string->number node-depth)))
		(if (eq? ndepth depth)
		    ;; This next is the match condition depth == node-depth
		    (if (eq? currnode nodenum)
			(begin
			  ;; (display " <X>")
			  (append path (list node-title)))
			(loop (+ currnode 1)
			      (+ depth 1)
			      (append path (list node-title))))
		    ;; didn't match, reset to base path and keep looking
		    ;; due to more iup odditys we don't reset to base
		    (begin 
		      ;; (display " <L>")
		      (loop (+ 1 currnode)
			    2
			    (append (take path ndepth)(list node-title)))))))))))

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

(test #f '("Figures")             (tree-node->path t 0))
(test #f '("Figures" "d")         (tree-node->path t 1))
(test #f '("Figures" "d" "b" "c") (tree-node->path t 3))
(test #f '("Figures" "a")         (tree-node->path t 4))
(test #f '("Figures" "a" "b" "c")     (tree-node->path t 8)) 
(test #f '()                      (tree-node->path t 40))

(iup:main-loop)

