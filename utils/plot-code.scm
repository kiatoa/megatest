#!/mfs/pkgs/chicken/4.8.0.5/bin/csi -nbq

(use regex srfi-69)

(define targs (string-split (cadddr (argv)) ","))
(define files (cddddr (argv)))

(define filedat-defns (make-hash-table))
(define filedat-usages (make-hash-table))

(define defn-rx (regexp "^\\s*\\(define\\s+\\(([^\\s\\)]+).*"))
(define all-regexs (make-hash-table))

(define all-fns '())

(define (print-err . data)
  (with-output-to-port (current-error-port)
    (lambda ()
      (apply print data))))

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

(print-err "Make the quick check regex")
(define have-function-rx (regexp (conc "(" (string-intersperse all-fns "|")
				       ")")))

(define (look-for-all-calls inl fnname)
  (if (string-search have-function-rx inl)
      (let loop ((hed (car all-fns))
		 (tal (cdr all-fns))
		 (res '()))
	(let ((match (string-match (hash-table-ref all-regexs fnname) inl)))
	  (if match
	      (let ((newres (cons hed res)))
		(if (not (null? tal))
		    newres
		    (loop (car tal)
			  (cdr tal)
			  newres)))
	      (if (null? tal)
		  res
		  (loop (car tal)(cdr tal) res)))))
      '()))
  
;; Gather the usages
(print "digraph G {")
(define curr-cluster-num 0)
(define function-calls '())

(for-each
 (lambda (fname)
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
		  (fnname "toplevel"))
	 (if (not (eof-object? inl))
	     (let ((match (string-match defn-rx inl)))
	       (if match
		   (let ((func-name (cadr match)))
		     (print "\"" func-name "\";")
		     (hash-table-set! breadcrumbs func-name #t)
		     (loop (read-line)
			   func-name))
		   (let ((calls (look-for-all-calls inl fnname)))
		     (if (not (null? calls))
			 (set! function-calls (cons (list fnname calls) function-calls)))
			 ;; (print "Function: " fnname " calls: " calls))
		     (loop (read-line) fnname))))))))
   (print "}"))
 targs)

(for-each 
 (lambda (function-call)
   (let ((fnname (car function-call))
	 (calls  (cadr function-call)))
     (for-each
      (lambda (callname)
	(print (if (hash-table-ref/default breadcrumbs callname #f) "" "// ")
	       "\"" fnname "\" -> \"" callname "\";"))
      calls)))
 function-calls)

(print "}")

;; (exit)