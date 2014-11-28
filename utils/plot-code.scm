#!/mfs/pkgs/chicken/4.8.0.5/bin/csi -nbq

;; Coming soon (right?) Usage: plot-code file1.scm,file2.scm "fun1,fun2,x*" *.scm > plot.dot
;; Usage: plot-code file1.scm,file2.scm *.scm > plot.dot
;;        dot -Tpdf plot.dot > plot.pdf
;; first param is comma separated list of files to include in the map, use - to do all
;; second param is list of regexs for functions to include in the map
;; third param is list of files to scan

(use regex srfi-69 srfi-13)

(define targs #f) 
(define files (cddddr (argv)))

(let ((targdat (cadddr (argv))))
  (if (equal? targdat "-")
      (set! targs files)
      (set! targs (string-split targdat ","))))

(define filedat-defns (make-hash-table))
(define filedat-usages (make-hash-table))

(define defn-rx (regexp "^\\s*\\(define\\s+\\(([^\\s\\)]+).*"))
(define all-regexs (make-hash-table))

(define all-fns '())

;; for the se

(define (print-err . data)
  (with-output-to-port (current-error-port)
    (lambda ()
      (apply print data))))

(print-err "Making graph for files: " (string-intersperse targs ", "))
(print-err "Looking at files: " (string-intersperse files ", "))

;; Gather the functions
;;
(for-each 
 (lambda (fname)
   (print-err "Processing file " fname)
   (with-input-from-file fname
     (lambda ()
       (let loop ((inl (read-line)))
	 (if (not (eof-object? inl))
	     (let ((match (string-match defn-rx inl)))
	       (if match 
		   (let ((fnname (cadr match)))
		     ;; (print "   " fnname)
		     (set! all-fns (cons fnname all-fns))
		     (hash-table-set! 
		      filedat-defns 
		      fname
		      (cons fnname (hash-table-ref/default filedat-defns fname '())))
		     ))
	       (loop (read-line))))))))
 files)

;; fill up the regex hash
(print-err "Make the huge regex hash")
(for-each
 (lambda (fnname)
   (hash-table-set! all-regexs fnname (regexp (conc "^(|.*[^a-zA-Z]+)" fnname "([^a-zA-Z]+|)$"))))
 (cons "toplevel" all-fns))

(define breadcrumbs (make-hash-table))

(define (have-function inl)
  (let loop ((hed (car all-fns))
	     (tal (cdr all-fns)))
    (if (string-contains inl hed)
	#t
	(if (null? tal)
	    #f
	    (loop (car tal)(cdr tal))))))

(define (look-for-all-calls inl fnname)
  (if (have-function inl) ;; (string-search have-function-rx inl)
      (let loop ((hed (car all-fns))
		 (tal (cdr all-fns))
		 (res '()))
	(let ((match (string-match (hash-table-ref all-regexs hed) inl)))
	  (if match
	      (let ((newres (cons hed res)))
		(if (null? tal)
		    newres
		    (loop (car tal)
			  (cdr tal)
			  newres)))
	      (if (null? tal)
		  res
		  (loop (car tal)(cdr tal) res)))))
      '()))

;; (define mm-header #<<MMHEADER
;; <map version="freeplane 1.2.0">
;; <!--To view this file, download free mind mapping software Freeplane from http://freeplane.sourceforge.net -->
;; 
;; MMHEADER
;; 
;; (define (add-node text)
;;   <node TEXT="homenode" ID="ID_1723255651" CREATED="1283093380553" MODIFIED="1417113442955"><hook NAME="MapStyle">
;; )
;; 
;;  minimal mindmap file
;;  <map version="freeplane 1.2.0">
;;   <!--To view this file, download free mind mapping software Freeplane from http://freeplane.sourceforge.net -->
;;   <node TEXT="homenode" ID="ID_1723255651" CREATED="1283093380553" MODIFIED="1417113442955">
;;   <node TEXT="node1" POSITION="right" ID="ID_1810107939" CREATED="1417113473476" MODIFIED="1417113480425">
;;   <node TEXT="node2" ID="ID_68133256" CREATED="1417113482134" MODIFIED="1417113484466"/>
;;   <node TEXT="node3" ID="ID_1572284821" CREATED="1417113487785" MODIFIED="1417113491589"/>
;;   </node>
;;   </node>
;;   </map>

;; Gather the usages
(print "digraph G {")
(define curr-cluster-num 0)
(define function-calls '())

(for-each
 (lambda (fname)
   (let ((last-func #f))
     (print-err "Processing file " fname)
     (print "subgraph cluster_" curr-cluster-num " {")
     (set! curr-cluster-num (+ curr-cluster-num 1))
     (with-input-from-file fname
       (lambda ()
	 (with-output-to-port (current-error-port)
	   (lambda ()
	     (print "Analyzing file " fname)))
	 (print "label=\"" fname "\";")
	 (let loop ((inl    (read-line))
		    (fnname "toplevel")
		    (allcalls '()))
	   (if (eof-object? inl)
	       (begin
		 (set! function-calls (cons (list fnname allcalls) function-calls))
		 (for-each 
		  (lambda (call-name)
		    (hash-table-set! breadcrumbs call-name #t))
		  allcalls)
		 (print-err "function: " fnname " allcalls: " allcalls))
	       (let ((match (string-match defn-rx inl)))
		 (if match
		     (let ((func-name (cadr match)))
		       (if last-func
			   (print "\"" func-name "\" -> \"" last-func "\";")
			   (print "\"" func-name "\";"))
		       (set! last-func func-name)
		       (hash-table-set! breadcrumbs func-name #t)
		       (loop (read-line)
			     func-name
			     allcalls))
		     (let ((calls (look-for-all-calls inl fnname)))
		       (loop (read-line) fnname (append allcalls calls)))))))))
     (print "}")))
 targs)

(print-err "breadcrumbs: " (hash-table-keys breadcrumbs))
(print-err "function-calls: " function-calls)

(for-each 
 (lambda (function-call)
   (print-err "function-call: " function-call)
   (let ((fnname (car function-call))
	 (calls  (cadr function-call)))
     (for-each
      (lambda (callname)
	(print (if (hash-table-ref/default breadcrumbs callname #f) "" "// ")
	       "\"" fnname "\" -> \"" callname "\";"))
      calls)))
 function-calls)

(print "}")

(exit)