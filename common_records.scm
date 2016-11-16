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

(include "altdb.scm")

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

;; iup callbacks are not dumping the stack, this is a work-around
;;
(define-simple-syntax (debug:catch-and-dump proc procname)
  (handle-exceptions
   exn
   (begin
     (print-call-chain (current-error-port))
     (with-output-to-port (current-error-port)
       (lambda ()
	 (print ((condition-property-accessor 'exn 'message) exn))
	 (print "Callback error in " procname)
	 (print "Full condition info:\n" (condition->list exn)))))
   (proc)))

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
  (cond
   ((and (number? *verbosity*)   ;; number number
	 (number? n))
    (<= n *verbosity*))
   ((and (list? *verbosity*)     ;; list   number
	 (number? n))
    (member n *verbosity*))
   ((and (list? *verbosity*)     ;; list   list
	 (list? n))
    (not (null? (lset-intersection! eq? *verbosity* n))))
   ((and (number? *verbosity*)
	 (list? n))
    (member *verbosity* n))))
      
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
  
(define (debug:print n e . params)
  (if (debug:debug-mode n)
      (with-output-to-port (or e (current-error-port))
	(lambda ()
	  (if *logging*
	      (db:log-event (apply conc params))
	      (apply print params)
	      )))))

;; Brandon's debug printer shortcut (indulge me :)
(define (BB> . in-args)
  (let* ((stack (get-call-chain))
         (location #f))
    (for-each
     (lambda (frame)
       (let* ((this-loc (vector-ref frame 0))
              (this-func (cadr (string-split this-loc " "))))
         (if (equal? this-func "BB>")
             (set! location this-loc))))
     stack)
    (let ((dp-args (append (list 0 *default-log-port* location"   "  ) in-args)))
      (apply debug:print dp-args))))

(define (debug:print-error n e . params)
  ;; normal print
  (if (debug:debug-mode n)
      (with-output-to-port (or e (current-error-port))
	(lambda ()
	  (if *logging*
	      (db:log-event (apply conc params))
	      ;; (apply print "pid:" (current-process-id) " " params)
	      (apply print "ERROR: " params)
	      ))))
  ;; pass important messages to stderr
  (if (and (eq? n 0)(not (eq? e (current-error-port)))) 
      (with-output-to-port (current-error-port)
	(lambda ()
	  (apply print "ERROR: " params)
	  ))))

(define (debug:print-info n e . params)
  (if (debug:debug-mode n)
      (with-output-to-port (or e (current-error-port))
	(lambda ()
	  (if *logging*
	      (let ((res (format#format #f "INFO: (~a) ~a" n (apply conc params))))
		(db:log-event res))
	      ;; (apply print "pid:" (current-process-id) " " "INFO: (" n ") " params) ;; res)
	      (apply print "INFO: (" n ") " params) ;; res)
	      )))))

;; if a value is printable (i.e. string or number) return the value
;; else return an empty string
(define-inline (printable val)
  (if (or (number? val)(string? val)) val ""))

