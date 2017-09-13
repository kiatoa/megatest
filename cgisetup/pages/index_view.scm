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

(define (pages:index session db shared)
  (let* ((dbh         (s:db))
	 (page-name   (sdat-get-page s:session)))
    (if (equal? page-name "api")
	(s:call page-name) ;; go straight to the api
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
                  (s:ul 'class "menu"
(s:li (s:a 'href ""  (s:i 'class "fa fa-inbox") "QA Summary")
      (s:ul
	(s:li (s:a 'href "/cgi-bin/megatest.sh/home"  "Component Snapshot"))
        (s:li (s:a 'href "/cgi-bin/megatest.sh/kitprogress"  "Kit/Contour progress"))
 )))
;(s:li (s:a 'href (s:link-to "run" ) "Runs"))) 
		  (case (string->symbol page-name)
		    ((index)  (s:call "home"))
		    (else     (s:call page-name))))
	   index:jquery
	   index:javascript
	   ))))))
