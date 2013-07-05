;;======================================================================
;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

(use format)
(require-library iup)
(import (prefix iup iup:))
(use canvas-draw)

(use sqlite3 srfi-1 posix regex regex-case srfi-69)
(import (prefix sqlite3 sqlite3:))

(declare (unit tree))
(declare (uses margs))
(declare (uses launch))
(declare (uses megatest-version))
(declare (uses gutils))
(declare (uses db))
(declare (uses server))
(declare (uses synchash))
(declare (uses dcommon))

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")

;;======================================================================
;; T R E E   S T U F F 
;;======================================================================

;; path is a list of nodes, each the child of the previous
;; this routine returns the id so another node can be added
;; either as a leaf or as a branch
;;
;; BUG: This needs a stop sensor for when a branch is exhausted
;;
(define (tree:find-node obj path)
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
(define (tree:add-node obj top nodelst #!key (userdata #f))
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
	     (parentnode (tree:find-node obj pathl))
	     (nodenum    (tree:find-node obj newpath)))
	;; Add the branch under lastnode if not found
	(if (not nodenum)
	    (begin
	      (iup:attribute-set! obj (conc "ADDBRANCH" parentnode) hed)
	      (if userdata
		  (iup:attribute-set! obj (conc "USERDATA"   parentnode) userdata))
	      (if (null? tal)
		  #t
		  ;; reset to top
		  (loop (car nodelst)(cdr nodelst) 1 (list top)))) 
	    (if (null? tal) ;; if null here then this path has already been added
		#t
		(loop (car tal)(cdr tal)(+ depth 1) newpath))))))))

(define (tree:node->path obj nodenum)
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

