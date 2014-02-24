;;======================================================================
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

(define (debug:calc-verbosity vstr)
  (cond
   (vstr
    (let ((debugvals (string-split vstr ",")))
      (if (> (length debugvals) 1)
	  (map string->number debugvals)
	  (string->number (car debugvals)))))
   ((args:get-arg "-v")    2)
   ((args:get-arg "-q")    0)
   (else                   1)))

;; check verbosity, #t is ok
(define (debug:check-verbosity verbosity vstr)
  (if (not (or (number? verbosity)
	       (list?   verbosity)))
      (begin
	(print "ERROR: Invalid debug value " vstr)
	#f)
      #t))

(define (debug:debug-mode n)
  (or (and (number? *verbosity*)
	   (<= n *verbosity*))
      (and (list? *verbosity*)
	   (member n *verbosity*))))

(define (debug:setup)
  (let ((debugstr (or (args:get-arg "-debug")
		      (getenv "MT_DEBUG_MODE"))))
    (set! *verbosity* (debug:calc-verbosity debugstr))
    (debug:check-verbosity *verbosity* debugstr)
    (if (or (args:get-arg "-debug")
	    (not (getenv "MT_DEBUG_MODE")))
	(setenv "MT_DEBUG_MODE" (if (list? *verbosity*)
				    (string-intersperse (map conc *verbosity*) ",")
				    (conc *verbosity*))))))
  

(define (debug:print n . params)
  (if (debug:debug-mode n)
      (with-output-to-port (current-error-port)
	(lambda ()
	  (if *logging*
	      (db:log-event (apply conc params))
	      (apply print "pid:" (current-process-id) " " params)
	      )))))

(define (debug:print-info n . params)
  (if (debug:debug-mode n)
      (with-output-to-port (current-error-port)
	(lambda ()
	  (let ((res (format#format #f "INFO: (~2d) ~a" n (apply conc params))))
	    (if *logging*
		(db:log-event res)
		(apply print "pid:" (current-process-id) " " "INFO: (" n ") " params) ;; res)
		))))))

;; if a value is printable (i.e. string or number) return the value
;; else return an empty string
(define-inline (printable val)
  (if (or (number? val)(string? val)) val ""))

