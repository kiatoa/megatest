

(define (make-cached-writer the-db)
  (let ((db    the-db)
	(queue '()))
    (lambda (cacheable . qry-params) ;; fn qry
      (if cacheable
	  (begin
	    (set! queue (cons qry-params queue))
	    (call/cc))
	  (begin
	    (print "Starting transaction")
	    (for-each
	     (lambda (queue-item)
	       (let ((fn  (car queue-item))
		     (qry (cdr queue-item)))
		 (print "WRITE to " db ": " qry)
		 )
	     (reverse queue))
	    (print "End transaction")
	    (print "READ from " db ": " qry-params))))))

(define *cw* (make-cached-writer "the db"))

(define (dbcall cacheable query)
  (*cw* cacheable query))

(dbcall #t "insert abc")
(dbcall #t "insert def")
(dbcall #t "insert hij")
(dbcall #f "select foo")
