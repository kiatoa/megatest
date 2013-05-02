(use srfi-69)

(define (runs:queue-next-hed tal reg n regful)
  (if regful
      (car reg)
      (car tal)))

(define (runs:queue-next-tal tal reg n regful)
  (if regful
      tal
      (let ((newtal (cdr tal)))
	(if (null? newtal)
	    reg
	    newtal
	    ))))

(define (runs:queue-next-reg tal reg n regful)
  (if regful
      (cdr reg)
      (if (eq? (length tal) 1)
	  '()
	  reg)))

(use trace)
(trace runs:queue-next-hed
       runs:queue-next-tal
       runs:queue-next-reg)


(define tests '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))

(define test-registry (make-hash-table))

(define n 3)

(let loop ((hed   (car tests))
           (tal   (cdr tests))
           (reg   '()))
  (let* ((reglen (length reg))
	 (regful (> reglen n)))
    (print "hed=" hed ", length reg=" (length reg) ", (> lenreg n)=" (> (length reg) n))
    (let ((newtal (append tal (list hed)))) ;; used if we are not done with this test
      (cond
       ((not (hash-table-ref/default test-registry hed #f))
	(hash-table-set! test-registry hed #t)
	(print "Registering #" hed)
	(if (not (null? tal))
          (loop (runs:queue-next-hed tal reg n regful)
                (runs:queue-next-tal tal reg n regful)
		(let ((newl (append reg (list hed))))
		  (if regful
		      (cdr newl)
		      newl)))))
       (else
	(print "Running #" hed)
	(if (not (null? tal))
	    (loop (runs:queue-next-hed tal reg n regful)
		  (runs:queue-next-tal tal reg n regful)
		  (runs:queue-next-reg tal reg n regful))))))))
