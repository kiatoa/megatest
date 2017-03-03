;;======================================================================
;; Copyright 2017, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

;; a function <pagename>-action is called on POST

(define (home-action action)
  (case (string->symbol action)
    ((filter)
     (let ((target-type   (s:get-input 'target-type))
	   (target-filter (s:get-input 'tfilter))
	   (target        (s:get-input 'target))
	   (row-or-col    (s:get-input 'row-or-col)))
       ;;
       ;; s:set! is a page local var. Better than s:session-var-set! but still not a good idea.
       ;;
       (s:set! "row-or-col" (if (list? row-or-col)
				(string-intersperse row-or-col ",")
				row-or-col))
       (s:set! "target-type" target-type)
       (s:set! "tfilter" target-filter)
       (s:set! "target"  target)
       (s:set! "target-filter" target-filter)))))

