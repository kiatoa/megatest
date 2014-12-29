;;======================================================================
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

;; (use trace)

;; Some of these routines use:
;;
;;     http://www.cs.toronto.edu/~gfb/scheme/simple-macros.html
;;
;; Syntax for defining macros in a simple style similar to function definiton,
;;  when there is a single pattern for the argument list and there are no keywords.
;;
;; (define-simple-syntax (name arg ...) body ...)
;;

(define-syntax define-simple-syntax
  (syntax-rules ()
    ((_ (name arg ...) body ...)
     (define-syntax name (syntax-rules () ((name arg ...) (begin body ...)))))))

(define-syntax common:handle-exceptions
  (syntax-rules ()
    ((_ exn-in errstmt ...)(handle-exceptions exn-in errstmt ...))))

(define (debug:calc-verbosity vstr)
  (cond
   ((number? vstr) vstr)
   ((not (string?  vstr))   1)
   ;; ((string-match  "^\\s*$" vstr) 1)
   (vstr           (let ((debugvals  (filter number? (map string->number (string-split vstr ",")))))
		     (cond
		      ((> (length debugvals) 1) debugvals)
		      ((> (length debugvals) 0)(car debugvals))
		      (else 1))))
   ((args:get-arg "-v")   2)
   ((args:get-arg "-q")    0)
   (else                   1)))

;; check verbosity, #t is ok
(define (debug:check-verbosity verbosity vstr)
  (if (not (or (number? verbosity)
	       (list?   verbosity)))
      (begin
	(print "ERROR: Invalid debug value \"" vstr "\"")
	#f)
      #t))

(define (debug:debug-mode n)
  (or (and (number? *verbosity*)
	   (<= n *verbosity*))
      (and (list? *verbosity*)
	   (member n *verbosity*))))

(define (debug:setup)
  (let ((debugstr (or (args:get-arg "-debug")
		      (getenv "MT_DEBUG_MODE"))))
    (set! *verbosity* (debug:calc-verbosity debugstr))
    (debug:check-verbosity *verbosity* debugstr)
    ;; if we were handed a bad verbosity rule then we will override it with 1 and continue
    (if (not *verbosity*)(set! *verbosity* 1))
    (if (or (args:get-arg "-debug")
	    (not (getenv "MT_DEBUG_MODE")))
	(setenv "MT_DEBUG_MODE" (if (list? *verbosity*)
				    (string-intersperse (map conc *verbosity*) ",")
				    (conc *verbosity*))))))
  

(define (debug:print n . params)
  (if (debug:debug-mode n)
      (with-output-to-port (current-error-port)
	(lambda ()
	  (if *logging*
	      (db:log-event (apply conc params))
	      ;; (apply print "pid:" (current-process-id) " " params)
	      (apply print params)
	      )))))

(define (debug:print-info n . params)
  (if (debug:debug-mode n)
      (with-output-to-port (current-error-port)
	(lambda ()
	  (let ((res (format#format #f "INFO: (~2d) ~a" n (apply conc params))))
	    (if *logging*
		(db:log-event res)
		;; (apply print "pid:" (current-process-id) " " "INFO: (" n ") " params) ;; res)
		(apply print "INFO: (" n ") " params) ;; res)
		))))))

;; if a value is printable (i.e. string or number) return the value
;; else return an empty string
(define-inline (printable val)
  (if (or (number? val)(string? val)) val ""))

;;======================================================================
;; T E S T S U I T E   R E C O R D S
;;======================================================================

;; make-vector-record common_records testsuite toppath linktree configdat envvars
(define (make-common_records:testsuite)(make-vector 5))
(define-inline (common_records:testsuite-get-toppath     vec)    (vector-ref  vec 0))
(define-inline (common_records:testsuite-get-linktree    vec)    (vector-ref  vec 1))
(define-inline (common_records:testsuite-get-configdat   vec)    (vector-ref  vec 2))
(define-inline (common_records:testsuite-get-envvars     vec)    (vector-ref  vec 3))
(define-inline (common_records:testsuite-get-dbstruct    vec)    (vector-ref  vec 4))
(define-inline (common_records:testsuite-set-toppath!    vec val)(vector-set! vec 0 val))
(define-inline (common_records:testsuite-set-linktree!   vec val)(vector-set! vec 1 val))
(define-inline (common_records:testsuite-set-configdat!  vec val)(vector-set! vec 2 val))
(define-inline (common_records:testsuite-set-envvars!    vec val)(vector-set! vec 3 val))
(define-inline (common_records:testsuite-set-dbstruct!   vec val)(vector-set! vec 4 val))

(define (common_records:testsuite-add-envvar! vec var val)
  (let ((envvars (cons (cons var val) 
		       (or (common_records:testsuite-get-envvars vec) '()))))
    (common_records:testsuite-set-envvars! vec envvars)
    envvars))