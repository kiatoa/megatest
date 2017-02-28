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

;; index

(let* ((dbh      (s:db))
       (ttypes   (pgdb:get-target-types dbh))
       (selected (string->number (or (s:session-var-get "target-type") "0")))
       (tfilter  (or (s:session-var-get "target-filter") "%"))
       (targets  (pgdb:get-targets-of-type dbh selected tfilter))
       (target   (s:session-var-get "target")))
  (list
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
   (s:html
    (s:title (conc "Megatest")) 
    (s:head
     index:kickstart-junk
     ) 
    (s:body
     (s:div 'class "grid flex" 'id "top_of_page"
	    ;; add visible to columns to help visualize them e.g. "col_12 visible"
	    ;; BEGINNING OF HEADER
	    (s:div 'class "col_12"
		   (s:form
		    'action "index.filter" 'method "post"
		    (s:select (map (lambda (x)
				     (let ((tt-id (vector-ref x 0))
					   (ttype (vector-ref x 1)))
				       (if (eq? tt-id selected)
					   (list ttype tt-id ttype #t)
					   (list ttype tt-id ttype #f))))
				   ttypes)
			      'name 'target-type)
		    (s:input-preserve 'name "tfilter" 'placeholder "Filter targets")
		    (s:select (map (lambda (x)
				     (let ((t (vector-ref x 0)))
				       (list t t t (equal? t target))))
				   targets)
			      'name  'target)
		    (s:input 'type "submit" 'name "set-filter-vals" 'value "Submit" 'class "col_3")
		    ;; (s:h1 (s:session-var-get "target-type"))
		    (map (lambda (area)
			   (s:p "data=" (conc area)))
			 ;; (pgdb:get-tests dbh (or target "%"))
			 (pgdb:get-stats-given-target dbh (or target "%"))
			 )
		    index:jquery
		    index:javascript
		    )))))))

