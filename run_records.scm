(define-inline (test:get-id vec)       (vector-ref vec 0))
(define-inline (test:get-run_id vec)   (vector-ref vec 1))
(define-inline (test:get-test-name vec)(vector-ref vec 2))
(define-inline (test:get-state vec)    (vector-ref vec 3))
(define-inline (test:get-status vec)   (vector-ref vec 4))
(define-inline (test:get-item-path vec)(vector-ref vec 5))

(define-inline (test:test-get-fullname test)
   (conc (db:test-get-testname test)
	 (if (equal? (db:test-get-item-path test) "")
	     ""
	     (conc "(" (db:test-get-item-path test) ")"))))

