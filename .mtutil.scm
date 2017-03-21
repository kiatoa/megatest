
(use json)
(use ducttape-lib)

(define (get-last-runname area-path target)
  (let* ((run-data     (with-input-from-pipe (conc "megatest -list-runs % -target " target " -fields runs:runname,event_time -dumpmode sexpr -start-dir " area-path)
			 read)))
    (if (or (not run-data)
	    (null? run-data))
	#f
	(let* ((name-time    (let ((dat (map cdadr (alist-ref target run-data equal?)))) ;; (("runname" . "2017w07.0-0047") ("event_time" . "1487490424"))
			       ;; (print "dat=" dat)
			       (map (lambda (item)
				      (cons (alist-ref "runname" item equal?)
					    (string->number (alist-ref "event_time" item equal?))))
				    dat)))
	       (sorted       (sort name-time (lambda (a b)(> (cdr a)(cdr b)))))
	       (last-name    (if (null? sorted)
				 #f
				 (caar sorted))))
	  last-name))))
 
;; example of how to set up and write target mappers
;;
(hash-table-set! *target-mappers*
		 'prefix-contour
		 (lambda (target run-name area area-path reason contour mode-patt)
		   (conc contour "/" target)))
(hash-table-set! *target-mappers*
		 'prefix-area-contour
		 (lambda (target run-name area area-path reason contour mode-patt)
		   (conc area "/" contour "/" target)))
  
(hash-table-set! *runname-mappers*
		 'corporate-ww
		 (lambda (target run-name area area-path reason contour mode-patt)
		   (let* ((last-name   (get-last-runname area-path target))
			  (last-letter (if (string? last-name)
					   (let ((len (string-length last-name)))
					     (substring last-name (- len 1) len))
					   "a"))
			  (next-letter (list->string
					(list
					 (integer->char
					  (+ (char->integer (string-ref last-letter 0)) 1)))))) ;; surely there is an easier way?
		     ;; (print "last-name: " last-name " last-letter: " last-letter " next-letter: " next-letter)
		     (conc (seconds->wwdate (current-seconds)) next-letter))))

(print "Got here!")

