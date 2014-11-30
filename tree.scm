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
  (if (or (not (string? (iup:attribute obj "TITLE0")))
	  (string-null? (iup:attribute obj "TITLE0")))
      (iup:attribute-set! obj "ADDBRANCH0" top))
  (cond
   ((not (equal? top (iup:attribute obj "TITLE0")))
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
		;; ERROR? ADDING DATA TO PARENT, DONT WE WANT IT ON CREATED NODE?
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
  (let loop ((currnode 0)
	     (path     '()))
    (let* ((node-depth (string->number (iup:attribute obj (conc "DEPTH" currnode))))
	   (node-title (iup:attribute obj (conc "TITLE" currnode)))
	   (trimpath   (if (and (not (null? path))
				(> (length path) node-depth))
			   (take path node-depth)
			   path))
	   (newpath    (append trimpath (list node-title))))
      (if (>= currnode nodenum)
	  newpath
	  (loop (+ currnode 1)
		newpath)))))

(define (tree:delete-node obj top node-path) ;; node-path is a list of strings
  (let ((id  (tree:find-node obj (cons top node-path))))
    (print "Found node to remove " id " for path " top " " node-path)
    (iup:attribute-set! obj (conc "DELNODE" id) "SELECTED")))
	
#|

  (let* ((tb      (iup:treebox
                   #:value 0
                   #:name "Runs"
                   #:expand "YES"
                   #:addexpanded "NO"
                   #:selection-cb
                   (lambda (obj id state)
                     ;; (print "obj: " obj ", id: " id ", state: " state)
                     (let* ((run-path (tree:node->path obj id))
                            (run-id   (tree-path->run-id (cdr run-path))))
                       (if run-id
                           (begin
                             (dboard:data-set-curr-run-id! *data* run-id)
                             (dashboard:update-run-summary-tab)))
                       ;; (print "path: " (tree:node->path obj id) " run-id: " run-id)
                       ))))
|#
