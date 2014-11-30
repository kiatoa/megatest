;;;; trace.scm


(module trace (breakpoint 
	       trace untrace
	       break unbreak
	       trace-output-port
	       continue c 
	       traced?
	       trace-module untrace-module
	       trace-verbose
	       trace/untrace)
	       
(import scheme chicken csi)

(use advice extras ports data-structures)
(require-library srfi-1)
(import (except srfi-1 break) miscmacros)


(define *last-breakpoint* #f)
(define *traced-procedures* '())
(define *broken-procedures* '())
(define *trace-indent-level* 0)

(define trace-output-port (make-parameter (current-output-port)))
(define trace-verbose (make-parameter #t))

(define (break-entry name args)
  ;; Does _not_ unwind!
  (##sys#call-with-current-continuation
   (lambda (c)
     (let ((exn (##sys#make-structure
		 'condition
		 '(exn breakpoint)
		 (list '(exn . message) "*** breakpoint ***"
		       '(exn . arguments) (list (cons name args))
		       '(exn . location) name
		       '(exn . continuation) c) ) ) )
       (set! *last-breakpoint* exn)
       (signal exn) ) ) ) )

(define (break-resume exn)
  (let ((a (member '(exn . continuation) (##sys#slot exn 2))))
    (if a
	((cadr a) (void))
	(error "condition has no continuation" exn) ) ) )

(define (breakpoint #!optional (name 'breakpoint))
  (break-entry name '()) )

(define (trace-indent)
  (let ((port (trace-output-port)))
    (do ((i (fxmin 3 *trace-indent-level*) (fx- i 1)))
	((fx<= i 0))
      (write-char #\space port) )
    (fprintf port "[~a] " *trace-indent-level*) ) )

(define (traced-procedure-entry name args)
  (let ((port (trace-output-port)))
    (trace-indent)
    (set! *trace-indent-level* (fx+ 1 *trace-indent-level*))
    (write (cons name args) port)
    (write ", Called from: " port)
    (write (conc (car (reverse (get-call-chain)))))
    (write-char #\newline port)
    (flush-output port) ) )

(define (traced-procedure-exit name results)
  (let ((port (trace-output-port)))
    (set! *trace-indent-level* (fx- *trace-indent-level* 1))
    (trace-indent)
    (fprintf port "~a -> " name)
    (if results
	(for-each
	 (lambda (x)
	   (write x port)
	   (write-char #\space port) )
	 results)
	(display "(escaping)" port))
    (write-char #\newline port)
    (flush-output port) ) )

(define (procedure-name proc)
  (cond ((procedure-information proc) =>
	 (lambda (info)
	   (if (pair? info) (car info) info) ) )
	(else '<unknown>)) )

(define (do-trace procs)
  (for-each
   (lambda (s)
     (ensure procedure? s)
     (cond ((traced? s)
	    (warning "procedure already traced" s) )
	   (else
	    (let ((name (procedure-name s)))
	      (when (trace-verbose)
		(fprintf (current-error-port) "; tracing ~a~%" name))
	      (set! *traced-procedures* (cons (cons s name) *traced-procedures*))
	      (advise 
	       'around s
	       (lambda (next args)
		 (let ((results #f))
		   (dynamic-wind
		       (cut traced-procedure-entry name args)
		       (lambda () 
			 (call-with-values (cut apply next args)
			   (lambda rs
			     (set! results rs)
			     (apply values rs))))
		       (cut traced-procedure-exit name results))))
	       '*trace*)))))
   procs) )

(define (do-untrace-all)
  (define (unadvise* p)
    (ignore-errors (unadvise p '*trace*)))
  (for-each
   (lambda (proc)
     (let ((proc (car proc)))
       (when (trace-verbose)
	 (fprintf (current-error-port) "; untracing ~a~%" (procedure-name proc))
	 (unadvise* proc))))
   *traced-procedures*)
  (set! *traced-procedures* '()))

(define (do-untrace procs)
  (for-each
   (lambda (s)
     (ensure procedure? s)
     (let ((p (assq s *traced-procedures*)) 
	   (name (procedure-name s)))
       (cond ((not p) (warning "procedure not traced" name))
	     (else
	      (when (trace-verbose)
		(fprintf (current-error-port) "; untracing ~a~%" name))
	      (ignore-errors (unadvise s '*trace*))
	      (set! *traced-procedures* 
		(delete 
		 p *traced-procedures* 
		 eq?))))))
   procs) )

(define (do-break procs)
  (for-each
   (lambda (s)
     (let ((name (procedure-name s)))
       (ensure procedure? s)
       (cond ((assq s *broken-procedures*)
	      (warning "procedure already has break-point" name))
	     (else
	      (when (trace-verbose)
		(fprintf (current-error-port) "; setting break-point in ~a~%" name))
	      (set! *broken-procedures* (cons (cons s name) *broken-procedures*))
	      (advise 
	       'before s
	       (lambda (args)
		 (break-entry name args) )
	       '*break*) ) )))
   procs) )

(define (do-unbreak procs)
  (for-each
   (lambda (s)
     (ensure procedure? s)
     (let ((p (assq s *broken-procedures*)) 
	   (name (procedure-name s)))
       (cond ((not p) (warning "procedure has no breakpoint" name))
	     (else
	      (when (trace-verbose)
		(fprintf (current-error-port) "; removing break-point in ~a~%" name))
	      (ignore-errors (unadvise s '*break*))
	      (set! *broken-procedures* (delete p *broken-procedures* eq?) ) ) ) ) )
   procs) )

(define (do-unbreak-all)
  (for-each
   (lambda (bp)
     (ignore-errors (unadvise (car bp) '*break*)))
   *broken-procedures*)
  (set! *broken-procedures* '())
  (void))

(define (trace . procs)
  (cond ((null? procs)
	 (when (pair? *traced-procedures*)
	   (printf "Traced:~%~%")
	   (for-each (lambda (p) (printf "  ~a~%" (cdr p))) *traced-procedures*)) )
	(else
	 (do-trace procs) ) ) )

(define (untrace . procs)
  (cond ((null? procs) (do-untrace-all))
	(else (do-untrace procs)))
  (void))

(define (break . procs)
  (cond ((null? procs)
	 (when (pair? *broken-procedures*)
	   (printf "Breakpoints:~%~%")
	   (for-each (lambda (p) (printf "  ~a~%" (cdr p))) *broken-procedures*)) )
	(else
	 (do-break procs) ) ) )

(define (unbreak . procs)
  (cond ((null? procs) (do-unbreak-all))
	(else (do-unbreak procs))))

(define (continue #!optional (bp *last-breakpoint*))
  (cond (*last-breakpoint*
	 (let ((exn *last-breakpoint*))
	   (set! *last-breakpoint* #f)
	   (break-resume exn) ) )
	(else (display "no breakpoint pending\n") ) ) )

(define c continue)

(define (traced? proc)
  (assq proc *traced-procedures*))

(define (trace/untrace . procs)
  (for-each
   (lambda (proc)
     ((if (traced? proc) do-untrace do-trace) (list proc)))
   procs))

(define (walk-module mname proc)
  (let* ((m (##sys#find-module mname))
	 (exps (nth-value 1 (##sys#module-exports m))))
    (for-each
     (lambda (exp)
       (let* ((realname (cdr exp))
	      (prim (get realname '##core#primitive)))
	 (if prim
	     (warning "export is a core-library primitive - not traced" (car exp))
	     (when (##sys#symbol-has-toplevel-binding? realname)
	       (let ((val (##sys#slot realname 0)))
		 (when (procedure? val)
		   (proc val)))))))
     exps)))

(define (trace-module . mnames)
  (for-each
   (lambda (mname)
     (walk-module mname trace))
   mnames))

(define (untrace-module . mnames)
  (for-each
   (lambda (mname)
     (walk-module 
      mname 
      (lambda (proc)
	(when (traced? proc)
	  (do-untrace (list proc))))))
   mnames))

)
