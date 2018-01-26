;; Run like this: ((adjust the "30" number to a value that fills memory on the machine you are using)
;; script -c "free -g ; utils/memproblem 30 -:hm128G" memclean.log

;; Fill the cache with something like this:
;; find /path/to/lots/of/files/ -type f -exec cat {} > /dev/null \;


(use posix numbers srfi-4)

(define num-iter (or (if (> (length (argv)) 2)
                        (string->number (cadr (argv)))
                        #f)
                    43)) ;; Gigs memory to try to allocate
;; (print "Allocating up to " memsize "G memory. Note that due to the usage of the heap this will actually use up to " (* 2 memsize) "G")

(define (get-free)
  (let ((indat (with-input-from-pipe
                "free"
                read-lines)))
    (map string->number
         (cdr 
          (string-split
           (cadr indat))))))

(define-inline (cached dat)(list-ref dat 5))
(define-inline (used   dat)(list-ref dat 1))
(define-inline (free   dat)(list-ref dat 2))

(define-inline (k->G val)(/ val 1e6))
(define-inline (G->k val)(* val 1e6))

(define start-time (current-milliseconds))

(let loop ((n      0)
           (dat    (get-free))
           (stuff  '()))
  (let ((bigvec (make-u32vector 200000000))
        (startt (current-milliseconds)))
    (print "Value at 100: " (u32vector-ref bigvec 100) " ms to access: " (- (current-milliseconds) startt))
    (u32vector-set! bigvec (random 190000000) 111)
    (print n " Elapsed time: " (/ (- (current-milliseconds) start-time) 1000) " s "
           "Cached: " (k->G (cached dat)) " G "
           "Used:   " (k->G (used   dat)) " G ")
    (if (< n num-iter)
        (loop (+ n 1)(get-free) (cons bigvec stuff)))))

(exit)

