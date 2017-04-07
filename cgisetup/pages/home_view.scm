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
         (tab2-limit 15)
         (tab2-page   (if (or (equal? (s:get-param "page") "") (equal? (s:get-param "page") #f))
                      1
                        (string->number (s:get-param "page"))))
         
         (tab2-offset (- (* tab2-limit  tab2-page) tab2-limit))      
	 (ttypes      (pgdb:get-target-types dbh))
	 (selected    (string->number (or (s:get "target-type") "-1")))
         (target-slice (pgdb:get-distict-target-slice dbh)) 
         (selected-slice (if (s:get "tslice") 
                              (s:get "tslice")
                              (if  (s:get-param "tslice")
                                     (s:get-param "tslice")
                                   "")))  
	 (curr-trec   (filter (lambda (x)(eq? selected (vector-ref x 0))) ttypes))
	 (curr-ttype  (if (and selected
			       (not (null? curr-trec)))
			  (vector-ref (car curr-trec) 1) #f))
	 (all-parts   (if curr-ttype (append (string-split curr-ttype "/") '("runname" "testname")) '()))
          
	 (tfilter     (or (s:get "target-filter") "%"))
         (tslice-filter    (if (s:get "t-slice-patt") 
                                  (s:get "t-slice-patt")
                                 (if (s:get-param "patt") 
                                 (string-substitute  "x_x" "%"   (s:get-param "patt") 'all) 
                                  "%")
                                 ))
         (target-patt   (if (or (equal? selected-slice "") (equal? tslice-filter "" ))
                             "" 
                           (conc selected-slice "/" tslice-filter )))
         (tab2-data (if (equal? target-patt "")
                         `()
                         (pgdb:get-all-run-stats-target-slice dbh target-patt  tab2-limit tab2-offset)))
         (tab2-cnt (if (equal? target-patt "")
                         0
                         (pgdb:get-slice-cnt dbh target-patt)))

         (tab2-pages (round (/ tab2-cnt  tab2-limit))) 
         (tab2-page-lst (pgdb:get-pg-lst tab2-pages))
                       
         (tab2-ordered-data (pgdb:coalesce-runs-by-slice tab2-data selected-slice))  
 
	 (targets     (pgdb:get-targets-of-type dbh selected tfilter))
	 (row-or-col  (string-split (or (s:get "row-or-col") "") ","))
	 (all-data    (if (and selected
			       (not (eq? selected -1)))
                          (pgdb:get-latest-run-stats-given-target dbh selected tfilter)
                           '()  
			 ; (pgdb:get-stats-given-type-target dbh selected tfilter)
			 ; (pgdb:get-stats-given-target dbh tfilter)
			  ))
  (ordered-data (pgdb:coalesce-runs dbh all-data all-parts row-or-col 0)))
   (s:div 'class "col_12" 
        (s:ul 'class "tabs left"
      	(s:li (s:a 'href "#tabr1" "Sliced Filter"))
        (s:li (s:a 'href "#tabr2" "Genral Filter")))
  (s:div 'id "tabr1" 'class "tab-content"
      (s:div 'class "col_11" 
      (s:fieldset    "Filter Targets by slice"
	    (s:form
	     'action "home.filter2" 'method "post" 'name "form1"
	     (s:div 'class "col_12"
		    (s:div 'class "col_6"
			   (s:select (map (lambda (x)
					    (let ((t-slice (vector-ref x 0)))
					      (if (equal? t-slice selected-slice)
						  (list t-slice t-slice t-slice #t)
						  (list t-slice t-slice t-slice #f))))
					  target-slice)
				     'name 'tslice-select))
		    (s:div 'class "col_4"
			   (s:input 'type "text" 'name "t-slice-filter" 'value tslice-filter))
		    (s:div 'class "col_2"
			   (s:input 'type "submit" 'name "set-filter-vals" 'value "Submit")))))
      (s:br)
     ;  (s:p (conc tslice-filter selected-slice  tab2-page))   
      (s:p (map
            (lambda (i) 
          (s:span (s:a 'href (s:link-to "home" 'page i 'patt (string-substitute "%" "x_x" tslice-filter 'all)  'tslice selected-slice) "PAGE " i  )"&nbsp;|&nbsp;"))  
          tab2-page-lst))
         
      (s:p "&nbsp;&nbsp;Result Format: &nbsp;&nbsp;total / pass / fail / other")
      (s:fieldset	    (conc "Runs data for " target-patt) 
          (let* ((target-keys (hash-table-keys tab2-ordered-data))
		  (run-keys (delete-duplicates (apply  append (map (lambda (sub-key)
					 (let ((subdat (hash-table-ref  tab2-ordered-data sub-key)))
					   (hash-table-keys subdat)))
				       target-keys)))))
            (s:table  'class "striped"
		   (s:tr  (s:th  'class "heading" ) 
 			(map
                	(lambda (th-key) 
                         (s:th 'class "heading" th-key )) 
                    run-keys))
		   (map
		    (lambda (row-key)
		      (s:tr (s:td row-key)
			    (map
			     (lambda (col-key)
			       (let ((val (let* ((ht  (hash-table-ref/default  tab2-ordered-data row-key #f)))
					    (if ht (hash-table-ref/default ht col-key #f)))))
				 (if val
				     (let* ((total (vector-ref val 3))
					    (pass  (vector-ref val 4))
					    (fail  (vector-ref val 5))
					    (other (vector-ref val 6))
					    (passper (round (* (/ pass total) 100)))
					    (target-param (string-substitute "[/]" "_x_" (conc selected-slice "/" row-key) 'all)))
				       (s:td   'style (conc "background: -webkit-linear-gradient(left, green " passper "%, red); background: -o-linear-gradient(right, green " passper "%, red); background: -moz-linear-gradient(right, green " passper "%, red); background: linear-gradient(to right, green " passper "%, red);")
      				      (s:a 'class "white"  'href (s:link-to "run" 'target target-param 'run col-key)
(conc  total "/" pass "/" fail "/" other))))
				     (s:td ""))))
			     run-keys)))
		    target-keys))
))
))
    (s:div 'id "tabr2" 'class "tab-content"
      (s:div 'class "col_11"
	   (s:fieldset    "Area type and target filter"
	    (s:form
	     'action "home.filter#tabr2" 'method "post"
	     (s:div 'class "col_12"
		    (s:div 'class "col_6"
			   (s:select (map (lambda (x)
					    (if x
						(let ((tt-id (vector-ref x 0))
						      (ttype (vector-ref x 1)))
						  (if (eq? tt-id selected)
						      (list ttype tt-id ttype #t)
						      (list ttype tt-id ttype #f)))
						(list "all" -1 "all" (eq? selected -1))))
					  (cons #f ttypes))
				     'name 'target-type))
		    (s:div 'class "col_4"
			   (s:input-preserve 'name "tfilter" 'placeholder "Filter targets"))
		    (s:div 'class "col_2"
			   (s:input 'type "submit" 'name "set-filter-vals" 'value "Submit")))))
           (s:br) 
           (s:p "&nbsp;&nbsp;Result Format: &nbsp;&nbsp;total / pass / fail / other")
           	   (s:fieldset	    (conc "Runs data for " tfilter)
	    ;;
	    ;; A very basic display
	    ;;
	    (let* ((a-keys (pgdb:ordered-data->a-keys ordered-data))
		   (b-keys (pgdb:ordered-data->b-keys ordered-data a-keys)))
	      ;; (c-keys (delete-duplicates b-keys)))
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
		  (s:table  'class "striped"
		   (s:tr  (s:th  'class "heading" ) 
 			(map
                	(lambda (th-key) 
                         (s:th 'class "heading" th-key )) 
                    a-keys))
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
                                            (id (vector-ref val 5)) 
					    (passper (round (* (/ pass total) 100)))
					    (failper (- 100 passper))
                                             (history (pgdb:get-run-stats-history-given-target dbh selected (conc col-key "/" row-key)))  
         				     (history-hash (pgdb:get-history-hash history))
                                             (history-keys (sort (hash-table-keys history-hash) string>=?))
					    (run-key (string-substitute "[/]" "_x_" (conc col-key "/" row-key) 'all)))
				       (s:td   'style (conc "background: -webkit-linear-gradient(left, green " passper "%, red); background: -o-linear-gradient(right, green " passper "%, red); background: -moz-linear-gradient(right, green " passper "%, red); background: linear-gradient(to right, green " passper "%, red);")
      				      (s:a 'class "white"  'href (s:link-to "run" 'target run-key)
					  (conc "Latest:" total "/" pass "/" fail "/" other)) (s:span " | ") (s:a 'id id 'class "viewmodal"  'title "Click to see description"  "History") (s:br)
                                   (s:div 'id (conc "myModal" id) 'class "modal"
                                        (s:div 'class "modal-content"
                                             (s:span 'id id 'class "close" "&times;") 
    						;(s:p (conc "Modal " id ".."))
                                                 (s:div                                                  
                                                          (s:table 
                                                             (s:tr
                                                               (s:th "Runame")
                                                               (s:th "Result")
                                                               )
                                                            (map
			    					(lambda (history-key)
                                                                 (let* ((history-row (hash-table-ref/default history-hash history-key #f))
                                                                         (htotal (vector-ref history-row 1))
                                                                         (hpass (vector-ref history-row 2))
                                                                         (hfail (vector-ref history-row 3))
                                                                         (hother (vector-ref history-row 4))
                                                                         (passper (round (* (/ hpass htotal) 100))))
                                                                (s:tr (s:td  history-key)
                                                                      (s:td 'style (conc "background: -webkit-linear-gradient(left, green " passper "%, red); background: -o-linear-gradient(right, green " passper "%, red); background: -moz-linear-gradient(right, green " passper "%, red); background: linear-gradient(to right, green " passper "%, red);")
(conc  htotal "/" hpass "/" hfail "/" hother )))))
                                                              history-keys)))

))
 ))
				     (s:td ""))))
			     a-keys)))
		    b-keys)))))))
)))
