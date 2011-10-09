(use srfi-9)

(define numtodo (string->number (caddr (argv))))

;; using vectors
(define testvalvec (vector 0 1 2 3 4 5))
(define-inline (testing:get-first  vec    )(vector-ref  vec 0))
(define-inline (testing:get-count  vec    )(vector-ref  vec 5))
(define-inline (testing:set-first! vec val)(vector-set! vec 0 val))
(define-inline (testing:set-count! vec val)(vector-set! vec 5 val))

(if (equal? (cadr (argv)) "vectors")
    (begin
      (print "Testing " numtodo " vectors")
      (let loop ((i 0))	
        (testing:set-count! testvalvec i)
        (testing:set-first! testvalvec (testing:get-count testvalvec))
        (if (< i numtodo)
            (loop (+ i 1))))))

;; using records
(define-record-type  testing
  (make-testing zeroeth first second third fourth count)
  testing?
  (count get:count set:count)
  (first get:first set:first))

(define testvalrec (make-testing 0 1 2 3 4 5))

(if (equal? (cadr (argv)) "records")
    (begin
      (print "Testing " numtodo " records")
      (let loop ((i 0))
         (set:count testvalrec i)
         (set:first testvalrec (get:count testvalrec))
         (if (< i numtodo)
             (loop (+ i 1))))))
