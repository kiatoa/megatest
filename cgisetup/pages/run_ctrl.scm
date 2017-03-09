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

(define (run-action action)
  (case (string->symbol action)
    ((filter)
     (let ((run-name-filter (s:get-input 'run-name-filter))
            (target (s:get-input 'target)))
     (s:set! "run-name-filter" run-name-filter)
     (s:set! "target" target)))))


