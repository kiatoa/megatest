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

(require-library iup)
(import (prefix iup iup:))
(use canvas-draw)

(use srfi-1 regex regex-case srfi-69)
(declare (unit gutils))

(define (gutils:colors-similar? color1 color2)
  (let* ((c1 (map string->number (string-split color1)))
	 (c2 (map string->number (string-split color2)))
	 (delta (map (lambda (a b)(abs (- a b))) c1 c2)))
    (null? (filter (lambda (x)(> x 3)) delta))))

(define (gutils:get-color-for-state-status state status)
  (case (string->symbol state)
    ((COMPLETED)
     (if (equal? status "PASS")
	 "70 249 73"
	 (if (or (equal? status "WARN")
		 (equal? status "WAIVED"))
	     "255 172 13"
	     "223 33 49"))) ;; greenish orangeish redish
    ((LAUNCHED)         "101 123 142")
    ((CHECK)            "255 100 50")
    ((REMOTEHOSTSTART)  "50 130 195")
    ((RUNNING)          "9 131 232")
    ((KILLREQ)          "39 82 206")
    ((KILLED)           "234 101 17")
    ((NOT_STARTED)      "240 240 240")
    (else               "192 192 192")))

