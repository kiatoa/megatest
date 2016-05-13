(use iup)

(define *debugger-control* #f)
(define *debugger-rownum*  0)
(define *debugger-matrix*  #f)
(define *debugger*         #f)

(define (debugger)
  (if (not *debugger*)
      (set! *debugger* 
	    (thread-start!
	     (make-thread
	      (lambda ()
		(show
		 (dialog
		  (let ((pause #f)
			(mtrx  (matrix
				#:expand "YES"
				#:numlin 30
				#:numcol 3
				#:numlin-visible 20
				#:numcol-visible 3
				)))
		    (set! pause (button "Pause" 
					#:action (lambda (obj)
						   (set! *debugger-control* (not *debugger-control*))
						   (attribute-set! pause "BGCOLOR" (if *debugger-control*
										       "200 0 0"
										       "0 0 200")))))
		    (set! *debugger-matrix* mtrx)
		    (attribute-set! mtrx "WIDTH1" "200")
		    (vbox
		     mtrx
		     (hbox
		      pause)))))
		(main-loop)))))))

(define (debugger-start #!key (start 1))
  (set! *debugger-rownum* start))

(define (debugger-trace-var varname varval)
  (let ((oldval (attribute *debugger-matrix* (conc *debugger-rownum* ":1")))
	(newval (conc varval)))
    (if (not (equal? oldval newval))
	(begin
	  ;; (print "DEBUG: " varname " = " newval)
	  (attribute-set! *debugger-matrix* (conc *debugger-rownum* ":0") varname)
	  (attribute-set! *debugger-matrix* (conc *debugger-rownum* ":1") (conc varval))
	  ;; (attribute-set! *debugger-matrix* "FITTOTEXT" "C1")
	  ))
    (set! *debugger-rownum* (+ *debugger-rownum* 1))))


(define (debugger-pauser)
  (debugger)
  (attribute-set! *debugger-matrix* "REDRAW" "ALL")
  (let loop ()
    (if *debugger-control*
	(begin
	  (print "PAUSED!")
	  (thread-sleep! 1)
	  (loop))
	(thread-sleep! 0.01))))
		  
;;    ;; lets use the debugger eh?
;;    (debugger-start)
;;    (debugger-trace-var "can-run-more"     can-run-more)
;;    (debugger-trace-var "hed"              hed)
;;    (debugger-trace-var "prereqs-not-met"  (runs:pretty-string prereqs-not-met))
;;    (debugger-pauser)

