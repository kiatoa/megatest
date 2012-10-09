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

(define-inline (debug:print n . params)
  (begin
    (if (<= n *verbosity*)
	(apply print params))
    (if *logging*
	(apply db:log-event params))))

;; if a value is printable (i.e. string or number) return the value
;; else return an empty string
(define-inline (printable val)
  (if (or (number? val)(string? val)) val ""))

