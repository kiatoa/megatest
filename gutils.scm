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

(define gutils:colors
  '((PASS . "70 249 73")
    (FAIL . "253 33 49")
    (SKIP . "230 230 0")))

(define (gutils:get-color-spec effective-state)
  (or (alist-ref effective-state gutils:colors)
      (alist-ref 'FAIL gutils:colors)))
 
(define (gutils:get-color-for-state-status state status);; #!key (get-label #f))
  ;; ((if get-label cadr car)
  (case (string->symbol state)
    ((COMPLETED) ;; ARCHIVED)
     (case (string->symbol status)
       ((PASS)        (list "70  249 73" status))
       ((WARN WAIVED) (list "255 172 13" status))
       ((SKIP)        (list (gutils:get-color-spec 'SKIP) status))
       ((ABORT)       (list "198 36 166" status))
       (else (list "253 33 49" status))))
    ((ARCHIVED)
     (case (string->symbol status)
       ((PASS)        (list "70  170 73" status))
       ((WARN WAIVED) (list "200 130 13" status))
       ((SKIP)        (list (gutils:get-color-spec 'SKIP) status))
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
    ((NOT_STARTED)      (case (string->symbol status)
			  ((CHECK)(list (gutils:get-color-spec 'SKIP) state))
			  (else   (list "240 240 240"                 state))))
    ;; for xor mode below
    ;;
    ((CLEAN)
     (case (string->symbol status)
       ((CLEAN-FAIL CLEAN-CHECK CLEAN-ABORT)  (list "200 130 13" status)) ;; orange requested for these
       (else  (list "60  235 63" status))))
    ((DIRTY-BETTER)     (list "160  255 153" status))
    ((DIRTY-WORSE)      (list "165 42  42" status))
    ((BOTH-BAD)         (list "180 33 49" status))

    (else               (list "192 192 192"  state))))

