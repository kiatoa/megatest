
(use json fmt posix)

;; abstract out the alist-ref a bit and re-order the params
;;
(define-inline (aref dat key)
  (alist-ref key dat equal?))

;; convert silly vectors in json data to nice clean alist
;;
(define (to-alist inlst)
  (handle-exceptions
      exn
      (begin
	(print-call-chain)
	(print inlst))
    (cond
     ((proper-list? inlst) (map to-alist inlst))
     ((or (list? inlst) ;; it is a pair
	  (pair? inlst))   (cons (car inlst) (to-alist (cdr inlst))))
     ((vector? inlst)      (to-alist (vector->list inlst)))
     (else                 inlst))))

;; columnar line printer
;;
(define (print-rows inlist)
  (define (print-line x)
    (cat (car x)
	 (space-to 10)(pad/left 3 (cadr x))
	 (space-to 25)(pad/left 3 (caddr x))
	 ))
  (fmt #t (pad-char #\  (fmt-join/suffix print-line inlist nl))))

;; from the command line pull branch, start-tag, end-tag
;;
(define (extract-history branch start-tag end-tag)
  (let* ((data       (to-alist  ;; get all the data
		      (with-input-from-pipe
			  "fossil json timeline checkin -n 0"
			json-read)))
	 (timeline   (aref (aref data "payload") "timeline")) ;; extract the timeline alists
	 (start-flag #f)
	 (end-flag   #f))
    ;; now we have all needed data as a list of alists in time order, extract the
    ;; messages for given branch starting at start-tag and ending at end-tag
    (reverse ;; return results oldest to newest
     (filter
      (lambda (x) x)
      (map
       (lambda (entry)
	 (let ((tags (aref entry "tags")))
	   (if (or (not tags) ;; eh?
		   (not (list? tags)))
	       (begin
		 ;; (with-output-to-port (current-error-port)
		 ;;   (lambda ()
		 ;;     (print "ERROR: bad entry. tags: " tags)))
		 #f)
	       (let* ((btag (car tags))  ;; first tag is the primary branch
		      (tags (cdr tags))  ;; remainder are actual tags
		      (cmt  (aref entry "comment"))
		      (usr  (aref entry "user"))
		      (tms  (aref entry "timestamp")))
		 ;; (print "btag: " btag " tags: " tags " usr: " usr)
		 (if (equal? btag branch) ;; we are on the branch
		     (begin
		       (if (member start-tag tags)(set! start-flag #t))
		       (let ((res (if (and start-flag
					   (not end-flag))
				      `(,usr
					,(time->string (seconds->local-time tms) "WW%U.%w %H:%M")
					,cmt)
				      #f)))
			 (if (member end-tag tags)(set! end-flag #t))
			 res))
		     #f)))))
       (reverse timeline))))))

(define (process-fossil branch start-tag end-tag)
  (print-rows
   (extract-history branch start-tag end-tag)))

;; process command line args and dispatch the call to fossil processing
;;
(if (and (> (length (argv)) 3)
	 (< (length (argv)) 5))
    (apply process-fossil (cdr (argv)))
    (begin ;; no inputs, exit with message
      (print "Usage: fslrept branch start-tag end-tag")
      ))

