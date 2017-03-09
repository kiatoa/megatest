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
	 (target-param       (s:get-param 'target))   
         (target1      (if  (s:get "target") 
                       (s:get "target")
                       (s:get-param 'target)))
         (target (if (equal? target1 #f)
                     "%"
                    (string-substitute  "_x_"  "/" target1 'all)     
                    )) 
         (run-filter (or (s:get "run-name-filter") "%"))  
         (runs (pgdb:get-runs-by-target dbh target run-filter))
         (ordered-runs (pgdb:runs-to-hash runs)))
   
    (s:div 'class "col_12"
            (s:fieldset
	    "Run filter"
	    (s:form
	     'action "run.filter" 'method "post"
	     (s:div 'class "col_12"
		     (s:div 'class "col_6"
                           ;(s:p (conc "param" (s:get-param 'target)) )
                           ; (s:p (conc "get" (s:get "target")) )
                           ;(s:p target1)
			   (s:input-preserve 'name "run-name-filter" 'placeholder "Filter by run names")
                           (s:input 'type "hidden" 'value target 'name "target" ))

		    (s:div 'class "col_6"
			   (s:input 'type "submit" 'name "set-filter-vals" 'value "Submit")))
	     	     ))

	   (s:fieldset
	   (conc "Show a runs for Target: " target)
             (let* ((a-keys (sort (hash-table-keys ordered-runs) string>=?))
		   (b-keys (delete-duplicates(sort (apply
				  append
				  (map (lambda (sub-key)
					 (let ((subdat (hash-table-ref ordered-runs sub-key)))
					   (hash-table-keys subdat)))
				       a-keys))
				 string>=?))))
              
              (s:table
		   (s:tr  (s:th "") (map s:th a-keys))
		   (map
		    (lambda (row-key)
		      (s:tr (s:td row-key)
			    (map
			     (lambda (col-key)
			       (let ((val (let* ((ht  (hash-table-ref/default ordered-runs col-key #f)))
					    (if ht (hash-table-ref/default ht row-key #f)))))
				 (if val
				     (let* ((result (vector-ref val 2))
                                             (test-id (vector-ref val 4))
                                            (bg (if (equal? result "PASS")
                                                      "green"
                                                      "red")))
				       (s:td 'style (conc "background: " bg )
					     (s:a 'href (s:link-to "log" 'testid test-id)
						  result)))
				     (s:td ""))))
			     a-keys)))
		    b-keys)))))))
		      
