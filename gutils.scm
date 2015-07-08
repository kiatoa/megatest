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

(define (gutils:get-color-for-state-status state status);; #!key (get-label #f))
  ;; ((if get-label cadr car)
  (case (string->symbol state)
    ((COMPLETED) ;; ARCHIVED)
     (case (string->symbol status)
       ((PASS)        (list "70  249 73" status))
       ((WARN WAIVED) (list "255 172 13" status))
       ((SKIP)        (list "230 230 0" status))
       (else (list "253 33 49" status))))
    ((ARCHIVED)
     (case (string->symbol status)
       ((PASS)        (list "70  170 73" status))
       ((WARN WAIVED) (list "200 130 13" status))
       ((SKIP)        (list "180 180 0" status))
       (else (list "180 33 49" status))))
    ;;      (if (equal? status "PASS")
    ;;	  '("70 249 73" "PASS")
    ;;	  (if (or (equal? status "WARN")
    ;;		  (equal? status "WAIVED"))
    ;;	      (list "255 172 13" status)
    ;;	      (list "223 33 49"  status)))) ;; greenish orangeish redish
    ((LAUNCHED)         (list "101 123 142"  state))
    ((CHECK)            (list "255 100 50"   state))
    ((REMOTEHOSTSTART)  (list "50 130 195"   state))
    ((RUNNING)          (list "9 131 232"    state))
    ((KILLREQ)          (list "39 82 206"    state))
    ((KILLED)           (list "234 101 17"   state))
    ((NOT_STARTED)      (list "240 240 240"  state))
    (else               (list "192 192 192"  state))))

