

(define (make-cached-writer the-db)
  (let ((db    the-db)
	(queue '()))
    (lambda (cacheable . qry-params)
      (if cacheable
	  (set! queue (cons qry-params queue))
	  (begin
	    (print "Starting transaction")
	    (for-each
	     (lambda (queue-item)
	       (print "WRITE to " db ": " queue-item))
	     (reverse queue))
	    (print "End transaction")
	    (print "READ from " db ": " qry-params))))))

(define a (make-cached-writer "the db"))
(a #t "insert abc")
(a #t "insert def")
(a #t "insert hij")
(a #f "select foo")
