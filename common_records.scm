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

(use format)

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

(define-inline (debug:debug-mode n)
  (or (and (number? *verbosity*)
	   (<= n *verbosity*))
      (and (list? *verbosity*)
	   (member n *verbosity*))))

(define-inline (debug:print n . params)
  (if (debug:debug-mode n)
      (begin
	(apply print params)
	(if *logging* (apply db:log-event params)))))

(define-inline (debug:print-info n . params)
  (if (debug:debug-mode n)
      (let ((res (format#format #f "INFO:~2d ~a" n (apply conc params))))
	(print res)
	(if *logging* (db:log-event res)))))

;; if a value is printable (i.e. string or number) return the value
;; else return an empty string
(define-inline (printable val)
  (if (or (number? val)(string? val)) val ""))

