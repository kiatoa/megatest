;; Copyright 2007-2010, Matthew Welland.
;;
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;;
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(declare (unit margs))
(declare (uses common))

(define args:arg-hash (make-hash-table))

(define (args:get-arg arg . default)
  (if (null? default)
      (hash-table-ref/default args:arg-hash arg #f)
      (hash-table-ref/default args:arg-hash arg (car default))))

(define (args:get-arg-from ht arg . default)
  (if (null? default)
      (hash-table-ref/default ht arg #f)
      (hash-table-ref/default ht arg (car default))))

(define (args:usage . args)
  (if (> (length args) 0)
      (apply print "ERROR: " args))
  (if (string? help)
      (print help)
      (print "Usage: " (car (argv)) " ... "))
  (exit 0))

;; args: 
(define (args:get-args args params switches arg-hash num-needed)
  (let* ((numargs (length args))
	 (adj-num-needed (if num-needed (+ num-needed 2) #f)))
    (if (< numargs (if adj-num-needed adj-num-needed 2))
	(if (>= num-needed 1)
	    (args:usage "No arguments provided")
	    '())
	(let loop ((arg (cadr args))
		   (tail (cddr args))
		   (remargs '()))
	  (cond 
	   ((member arg params) ;; args with params
	    (if (< (length tail) 1)
		(args:usage "param given without argument " arg)
		(let ((val     (car tail))
		      (newtail (cdr tail)))
		  (hash-table-set! arg-hash arg val)
		  (if (null? newtail) remargs
		      (loop (car newtail)(cdr newtail) remargs)))))
	   ((member arg switches)         ;; args with no params (i.e. switches)
	    (hash-table-set! arg-hash arg #t)
	    (if (null? tail) remargs
		(loop (car tail)(cdr tail) remargs)))
	   (else
	    (if (null? tail)(append remargs (list arg)) ;; return the non-used args
		(loop (car tail)(cdr tail)(append remargs (list arg))))))))
    ))

(define (args:print-args remargs arg-hash)
  (print "ARGS: " remargs)
  (for-each (lambda (arg)
	      (print "   " arg "   " (hash-table-ref/default arg-hash arg #f)))
	    (hash-table-keys arg-hash)))
