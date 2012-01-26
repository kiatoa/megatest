;; make-vector-record tests testqueue testname testconfig waitons priority items
(define (make-tests:testqueue)(make-vector 6 #f))
(define-inline (tests:testqueue-get-testname     vec)    (vector-ref  vec 0))
(define-inline (tests:testqueue-get-testconfig   vec)    (vector-ref  vec 1))
(define-inline (tests:testqueue-get-waitons      vec)    (vector-ref  vec 2))
(define-inline (tests:testqueue-get-priority     vec)    (vector-ref  vec 3))
;; items: #f=no items, list=list of items remaining, proc=need to call to get items
(define-inline (tests:testqueue-get-items        vec)    (vector-ref  vec 4))
(define-inline (tests:testqueue-get-itemdat      vec)    (vector-ref  vec 5))

(define-inline (tests:testqueue-set-testname!    vec val)(vector-set! vec 0 val))
(define-inline (tests:testqueue-set-testconfig!  vec val)(vector-set! vec 1 val))
(define-inline (tests:testqueue-set-waitons!     vec val)(vector-set! vec 2 val))
(define-inline (tests:testqueue-set-priority!    vec val)(vector-set! vec 3 val))
(define-inline (tests:testqueue-set-items!       vec val)(vector-set! vec 4 val))
(define-inline (tests:testqueue-set-itemdat!     vec val)(vector-set! vec 5 val))
