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

(define (pages:home session db shared)
  (let* ((dbh         (s:db))
	 (ttypes      (pgdb:get-target-types dbh))
	 (selected    (string->number (or (s:get "target-type") "0")))
	 (curr-trec   (filter (lambda (x)(eq? selected (vector-ref x 0))) ttypes))
	 (curr-ttype  (if (and selected
			       (not (null? curr-trec)))
			  (vector-ref (car curr-trec) 1) #f))
	 (all-parts   (if curr-ttype (append (string-split curr-ttype "/") '("runname" "testname")) '()))
	 (tfilter     (or (s:get "target-filter") "%"))
	 (targets     (pgdb:get-targets-of-type dbh selected tfilter))
	 ;; (target      (s:session-var-get "target"))
	 ;; (target-patt (or target "%"))
	 (row-or-col  (string-split (or (s:get "row-or-col") "") ","))
	 (all-data    (if selected (pgdb:get-stats-given-target dbh selected tfilter)
			  '()))
	 ;; (all-data    (pgdb:get-tests dbh tfilter))
	 (ordered-data (pgdb:coalesce-runs dbh all-data all-parts row-or-col 0)))
    
    (s:div 'class "col_12"
	   (s:fieldset
	    "Area type and target filter"
	    (s:form
	     'action "home.filter" 'method "post"
	     (s:div 'class "col_12"
		    (s:div 'class "col_6"
			   (s:select (map (lambda (x)
					    (let ((tt-id (vector-ref x 0))
						  (ttype (vector-ref x 1)))
					      (if (eq? tt-id selected)
						  (list ttype tt-id ttype #t)
						  (list ttype tt-id ttype #f))))
					  ttypes)
				     'name 'target-type))
		    (s:div 'class "col_4"
			   (s:input-preserve 'name "tfilter" 'placeholder "Filter targets"))
		    (s:div 'class "col_2"
			   (s:input 'type "submit" 'name "set-filter-vals" 'value "Submit")))
	     ;; use radio buttons to select whether to put this identifier in row or column.
	     ;; this seems clumsly and takes up a lot of screen realestate
	     ;; (s:div 'class "col_12"
	     ;; 	      (s:div 'class "col_1" "identifier")
	     ;; 	      (map (lambda (target-var)
	     ;; 		     (s:div 'class "col_1" target-var))
	     ;; 		   all-parts))
	     ;; (s:div 'class "col_12"
	     ;; 	      (s:div 'class "col_1" "row")
	     ;; 	      (map (lambda (target-var)
	     ;; 		     (s:div 'class "col_1" (s:input 'type "checkbox" 'name "row-or-col" 'value target-var
	     ;; 						    ;; this silly trick preserves the checkmark
	     ;; 						    (if (member target-var row-or-col) 'checked "")
	     ;; 						    "")))
	     ;; 		   all-parts))
	     ))
           (s:br) 
           (s:p "&nbsp;&nbsp;Result Format: &nbsp;&nbsp;total / pass / fail / other")

	   (s:fieldset
	    (conc "Runs data for " tfilter)
	    ;;
	    ;; A very basic display
	    ;;
            	    (let* ((a-keys (sort (hash-table-keys ordered-data) string>=?))
		   (b-keys (delete-duplicates(sort (apply
				  append
				  (map (lambda (sub-key)
					 (let ((subdat (hash-table-ref ordered-data sub-key)))
					   (hash-table-keys subdat)))
				       a-keys))
				 string>=?))))
                  ; (c-keys (delete-duplicates b-keys)))
               	      (if #f ;; swap rows/cols
		  (s:table
		   (s:tr (s:td "")(map s:tr b-keys))
		   (map
		    (lambda (row-key)
		      (let ((subdat (hash-table-ref ordered-data row-key)))
			(s:tr (s:td row-key)
			      (map
			       (lambda (col-key)
				 (s:td (let ((dat (hash-table-ref/default subdat col-key #f)))
					 (s:td (if dat
						   (list (vector-ref dat 0)(vector-ref dat 1))
						   "")))))
			       b-keys))))
		    a-keys))
               
		  (s:table
		   (s:tr (s:td "")(map s:td a-keys))
		   (map
		    (lambda (row-key)
		      (s:tr (s:td row-key)
			    (map
			     (lambda (col-key)
			       (let ((val (let* ((ht  (hash-table-ref/default ordered-data col-key #f)))
					    (if ht (hash-table-ref/default ht row-key #f)))))
				 (if val
				     (let* ((total (vector-ref val 1))
					    (pass  (vector-ref val 2))
					    (fail  (vector-ref val 3))
					    (other (vector-ref val 4))
					    (passper (round (* (/ pass total) 100)))
					    (failper (- 100 passper))
					    (run-key ;; (string-substitute ;; %2F = /
						     ;; "-" "%2D"
						      ;;(string-substitute "/" "%2F" (conc col-key "/" row-key) 'all)
						      (string-substitute "[/]" "_x_" (conc col-key "/" row-key) 'all)
						      ;; 'all)))
						      ))
				       (s:td 'style (conc "background: linear-gradient(to right, green " passper "%, red " failper "%);")
					     (s:a 'href (s:link-to "run" 'target run-key)
						  (conc total "/" pass "/" fail "/" other))))
				     (s:td ""))))
			     a-keys)))
		    b-keys))))))))
