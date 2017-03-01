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
	 (ttypes      (pgdb:get-target-types dbh))
	 (selected    (string->number (or (s:session-var-get "target-type") "0")))
	 (curr-trec   (filter (lambda (x)(eq? selected (vector-ref x 0))) ttypes))
	 (curr-ttype  (if (and selected
			       (not (null? curr-trec)))
			  (vector-ref (car curr-trec) 1) #f))
	 (all-parts   (if curr-ttype (append (string-split curr-ttype "/") '("runname" "testname")) '()))
	 (tfilter     (or (s:session-var-get "target-filter") "%"))
	 (targets     (pgdb:get-targets-of-type dbh selected tfilter))
	 ;; (target      (s:session-var-get "target"))
	 ;; (target-patt (or target "%"))
	 (row-or-col  (string-split (or (s:session-var-get "row-or-col") "") ","))
	 (all-data    (pgdb:get-stats-given-target dbh tfilter))
	 ;; (all-data    (pgdb:get-tests dbh tfilter))
	 (ordered-data (pgdb:coalesce-runs dbh all-data all-parts row-or-col)))
    
    (list
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
     (s:html
      (s:title (conc "Megatest")) 
      (s:head
       index:kickstart-junk
       ) 
      (s:body
       ;; (s:session-var-get "target-type")
       ;; (conc " selected = " selected ", ttypes = " ttypes ", curr-ttype = " curr-ttype ", curr-trec = " curr-trec)
       (conc (hash-table->alist ordered-data))
       (s:div 'class "grid flex" 'id "top_of_page"
	      ;; add visible to columns to help visualize them e.g. "col_12 visible"
	      ;; BEGINNING OF HEADER
	      (s:div 'class "col_12"
		     (s:fieldset
		      "Area type and target filter"
		      (s:form
		       'action "index.filter" 'method "post"
		       (s:div 'class "col_12"
			      (s:select (map (lambda (x)
					       (let ((tt-id (vector-ref x 0))
						     (ttype (vector-ref x 1)))
						 (if (eq? tt-id selected)
						     (list ttype tt-id ttype #t)
						     (list ttype tt-id ttype #f))))
					     ttypes)
					'name 'target-type)
			      (s:input-preserve 'name "tfilter" 'placeholder "Filter targets")
			      (s:input 'type "submit" 'name "set-filter-vals" 'value "Submit" 'class "col_3"))
		       ;; use radio buttons to select whether to put this identifier in row or column.
		       ;; this seems clumsly and takes up a lot of screen realestate
		       (s:div 'class "col_12"
			      (s:div 'class "col_1" "identifier")
			      (map (lambda (target-var)
				     (s:div 'class "col_1" target-var))
				   all-parts))
		       (s:div 'class "col_12"
			      (s:div 'class "col_1" "row")
			      (map (lambda (target-var)
				     (s:div 'class "col_1" (s:input 'type "checkbox" 'name "row-or-col" 'value target-var
								    ;; this silly trick preserves the checkmark
								    (if (member target-var row-or-col) 'checked "")
								    "")))
				   all-parts))))
		     (s:fieldset
		      (conc "Runs data for " tfilter)
		      ;;
		      ;; This is completely wrong!!! However it may provide some ideas!
		      ;;
		      (s:table
		       (map
			(lambda (key)
			  (let ((subdat (hash-table-ref ordered-data key)))
			    (s:tr (s:td key)
				  (map
				   (lambda (remkey)
				     (s:td remkey
					   (let ((dat (hash-table-ref subdat remkey)))
					     (s:td (vector-ref dat 1) (vector-ref dat 0)))))
				   (sort (hash-table-keys subdat) string>=?)))))
			(sort (hash-table-keys ordered-data) string>=?)))
		      
		;;(map (lambda (area)
		;;	     (s:p "data=" (conc area)))
		;;	   ;; (pgdb:get-tests dbh tfilter))
		;;	   (pgdb:get-stats-given-target dbh tfilter))
			   


			   
		      index:jquery
		      index:javascript
		      ))))))))

  
		       ;; 	  (s:div 'class "col_12"
		       ;; 		 (s:div 'class "col_1" "row")
		       ;; 		 (map (lambda (target-var)
		       ;; 			(s:div 'class "col_1" (s:input 'type "radio" 'name target-var 'value "row")))
		       ;; 		      all-parts))
		       ;; 	  (s:div 'class "col_12"
		       ;; 		 (s:div 'class "col_1" "col")
		       ;; 		 (map (lambda (target-var)
		       ;; 			(s:div 'class "col_1" (s:input 'type "radio" 'name target-var 'value "col")))
		       ;; 		      all-parts)))
		       ;; 	 '())
		       ;; (s:h1 (s:session-var-get "target-type"))

	 ;; (s:select (map (lambda (x)
  ;; 		      (let ((t (vector-ref x 0)))
  ;; 			(list t t t (equal? t target))))
  ;; 		    targets)
  ;; 	       'name  'target)
