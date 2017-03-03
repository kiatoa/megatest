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

(define (pages:run session db shared)
  (let* ((dbh         (s:db))
	 (target      (s:get-param 'target)))
    
    (s:div 'class "col_12"
	   (s:fieldset
	    "Show a run"
	    target))))
		      
