(use regex srfi-69)

(define-record processdat
  %cpu
  virt
  res
  %mem
  count
  )

(define (snagload)
  (let ((dat (make-hash-table)) ;; user => hash-of-processdat
	(hdr (regexp "^\\s+PID"))
	(rx  (regexp "\\s+"))
	(wht (regexp "^\\s+"))
	)
    (with-input-from-pipe 
     "top -n 1 -b"
     (lambda ()
       (let loop ((inl   (read-line))
		  (inbod #f))
	 (if (eof-object? inl)
	     dat
	     (if (not inbod)
		 (if (string-search hdr inl)
		     (loop (read-line) #t)
		     (loop (read-line) #f))
		 (let* ((lparts (map (lambda (x)
				       (let ((num (string->number x)))
					 (if num num x)))
				     (string-split-fields rx (string-substitute wht "" inl) #:infix)))
			(user   (list-ref lparts 1))
			(virt   (list-ref lparts 4))
			(res    (list-ref lparts 5))
			(%cpu   (list-ref lparts 8))
			(%mem   (list-ref lparts 9))
			(time   (list-ref lparts 10))
			(pname  (list-ref lparts 11))
			(udat   (or (hash-table-ref/default dat user #f)
				    (let ((u (make-hash-table)))
				      (hash-table-set! dat user u)
				      u)))
			(pdat   (or (hash-table-ref/default udat pname #f)
				    (let ((p (make-processdat 0 0 0 0 0)))
				      (hash-table-set! udat pname p)
				      p))))
		   (print "User: " user ", pname: " pname ", virt: " virt ", res: " res ", %cpu: " %cpu ", %mem: " %mem)
		   (processdat-%cpu-set!  pdat (+ (processdat-%cpu pdat) %cpu))
		   (processdat-%mem-set!  pdat (+ (processdat-%mem pdat) %mem))
		   (processdat-virt-set!  pdat (+ (processdat-virt pdat) virt))
		   (processdat-res-set!   pdat (+ (processdat-res pdat)  res))
		   (processdat-count-set! pdat (+ (processdat-count pdat) 1))
		   (loop (read-line) inbod)))))))))
	   
(define x (snagload))
(processdat-%cpu (hash-table-ref (hash-table-ref x "matt") "evolution-calen"))