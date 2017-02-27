;; Copyright 2007-2008, Matthew Welland. Megatest All rights reserved.
;; 
;; index

(let ((dbh (s:db)))
  (list
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
   (s:html
    (s:title (conc "Megatest ")) 
    (s:head
     index:kickstart-junk
     ) 
    (s:body
     (s:div 'class "grid flex" 'id "top_of_page"
	    ;; add visible to columns to help visualize them e.g. "col_12 visible"
	    ;; BEGINNING OF HEADER
	    (s:div 'class "col_12"
		   (map (lambda (area)
			  (s:p "data=" (conc area)))
			;; (pgdb:get-tests dbh "%")
			(pgdb:get-stats-given-target dbh "v1.63/%")
			)
		   index:jquery
		   index:javascript
		   ))))))

