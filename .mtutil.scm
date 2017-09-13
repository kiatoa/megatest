
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

(define (str-first-char->number str)
  (char->integer (string-ref str 0)))
 
;; example of how to set up and write target mappers
;;
(add-target-mapper 'prefix-contour
		   (lambda (target run-name area area-path reason contour mode-patt)
		     (conc contour "/" target)))
(add-target-mapper 'prefix-area-contour
		   (lambda (target run-name area area-path reason contour mode-patt)
		     (conc area "/" contour "/" target)))
  
(add-runname-mapper 'corporate-ww
		    (lambda (target run-name area area-path reason contour mode-patt)
		      (print "corporate-ww called with: target=" target " run-name=" run-name " area=" area " area-path=" area-path " reason=" reason " contour=" contour " mode-patt=" mode-patt)
		      (let* ((last-name   (get-last-runname area-path target))
			     (last-letter (let* ((ch (if (string? last-name)
							 (let ((len (string-length last-name)))
							   (substring last-name (- len 1) len))
							 "a"))
						 (chnum (str-first-char->number ch))
						 (a     (str-first-char->number "a"))
						 (z     (str-first-char->number "z")))
					    (if (and (>= chnum a)(<= chnum z))
						chnum
						#f)))
			     (next-letter (if last-letter
					      (list->string
					       (list
						(integer->char
						 (+ last-letter 1)))) ;; surely there is an easier way?
					      "a")))
			;; (print "last-name: " last-name " last-letter: " last-letter " next-letter: " next-letter)
			(conc (seconds->wwdate (current-seconds)) next-letter))))

(add-runname-mapper 'auto
		    (lambda (target run-name area area-path reason contour mode-patt)
		      "auto-eh"))

;; run only areas where first letter of area name is "a"
;;
(add-area-checker 'first-letter-a
                  (lambda (area target contour)
                    (string-match "^a.*$" area)))


