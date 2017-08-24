(use posix srfi-69)

(define *numsamples* (or (and (> (length (argv)) 1)
                              (string->number (cadr (argv))))
                         20))

(print "Using " *numsamples* " as number of samples.")

(define (topdata)
  (with-input-from-pipe
   (conc "top -b -n " *numsamples* " -d 0.2")
   read-lines))

(define (cleanup-data topdat)list
  (let loop ((hed (car topdat))
              (tal (cdr topdat))
              (res '()))
    (let* ((line-list (string-split hed))
           (nums      (map (lambda (indat)(or (string->number indat) indat)) line-list))
           (not-data  (or (null? nums)
                          (not (number? (car nums)))))
           (new-res   (if not-data res (cons nums res))))
      (if (null? tal)
          new-res
          (loop (car tal)(cdr tal) new-res)))))

(define data (cleanup-data (topdata)))
(define pidhash  (make-hash-table))
(define userhash (make-hash-table))

;; sum up and normalize the 
(for-each
 (lambda (indat)
   (let ((pid (car indat))
         (usr (cadr indat))
         (cpu (list-ref indat 8)))
     (hash-table-set! userhash usr (+ cpu (hash-table-ref/default userhash usr 0)))))
 data)

(for-each
 (lambda (usr)
   (print usr
          (if (< (string-length usr) 8) "\t\t" "\t")
          (inexact->exact (round (/ (hash-table-ref userhash usr) *numsamples*)))))
 (sort (hash-table-keys userhash)
       (lambda (a b)
         (> (hash-table-ref userhash a)
            (hash-table-ref userhash b)))))

