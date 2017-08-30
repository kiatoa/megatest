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
     (let ((dot   (s:get-input 'dot))
	   (type (s:get-input 'kit-type))
	   (rel        (s:get-input 'rel-num))
           (bp (s:get-input 'bp)))
       ;;
       ;; s:set! is a page local var. Better than s:session-var-set! but still not a good idea.
       ;;
       
       (s:set! "dot" dot)
       (s:set! "type"  type)
       (s:set! "bp"  bp)

       (s:set! "rel" rel)))))

