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
         (limit 50)
         (curr-page   (if (or (equal? (s:get-param "pg") "") (equal? (s:get-param "pg") #f))
                      1
                        (string->number (s:get-param "pg"))))
         
         (offset (- (* limit  curr-page) limit))     
         (dot    (if (s:get-param "dot")
                           (string->number (s:get-param "dot"))
                           (if (and  (s:get "dot") (not (equal? (s:get "dot") "all")))
                             (string->number (s:get "dot"))
                              "all")))
         (type    (if (s:get-param "type")
                           (s:get-param "type")
                       (if (and (s:get "type") (not (equal? (s:get "type") "all")))
                              (s:get "type")
                              "all")))
          (bp    (if (s:get-param "bp")
                           (s:get-param "bp")
                       (if (s:get "bp") 
                              (s:get "bp")
                              "p1273")))
           (rel    (if (s:get-param "rel")
                           (s:get-param "rel")
                       (if (and  (s:get "rel") (not (equal? (s:get "rel") "all")))
                              (s:get "rel")
                              ""))) 
          (pattern  (pgdb:mk-pattern dot type bp rel)) 	 
	; (targets     (pgdb:get-targets-of-type dbh selected tfilter))
	            
	 (all-data       (pgdb:get-latest-run-stats-given-pattern dbh pattern  limit offset))
                           ;'()  )
			 ; (pgdb:get-stats-given-type-target dbh selected tfilter)
			 ; (pgdb:get-stats-given-target dbh tfilter)
			  
         (cnt     (pgdb:get-latest-run-cnt-by-pattern dbh pattern))
         (total-pages (ceiling (/ cnt  limit))) 
         (page-lst (pgdb:get-pg-lst total-pages))
         (ordered-data (pgdb:coalesce-runs1 all-data))
         (rel-val (if (equal? rel "")
                       "%"
                        rel)))
   (s:div 'class "col_12" 
        (s:ul 'class "tabs left"
          
        (map (lambda (x)
            	(s:li (s:a 'href (conc "#" x) x)))
	  *process*))
       (map (lambda (x)
        
       (s:div 'id  x 'class "tab-content"
      (s:div 'class "col_11"
	   (s:fieldset    "Area type and target filter"
	    (s:form
	     'action (conc "home.filter#" x) 'method "post"
	     (s:div 'class "col_12"
                        (s:div 'class "col_3"
			   (s:label "Release Type") (s:select (map (lambda (x)
                                           (if (equal?  x type)  
                                            (list x x x #t)
                                            (list x x x #f)) )
					  *kit-types*)
				     'name "kit-type"))
                   (s:div 'class "col_3"
			   (s:label "Dot") (s:select (map (lambda (x)
                                            (if (equal?  x dot)  
                                            (list x x x #t)
                                            (list x x x #f)))
					  *dots*)
				     'name "dot"))

		   (s:div 'class "col_3"
                            (s:input 'type "hidden" 'value x 'name "bp")
			   (s:label "Release #") (s:input 'type "text" 'name "rel-num" 'value rel-val))
		    (s:div 'class "col_2"
			   (s:input 'type "submit" 'name "set-filter-vals" 'value "Submit")))))
           (s:br)
           ;(s:p (conc dot(string? dot) )) 
             (s:p (map
            (lambda (i) 
          (s:span (s:a 'href (s:link-to "home" 'pg i ) "PAGE " i  )"&nbsp;|&nbsp;"))  
          page-lst))
           (s:p "&nbsp;&nbsp;Result Format: &nbsp;&nbsp;total / pass / fail / other")
            (if (equal? x bp)
             (begin 
           (s:fieldset	    (conc "Runs data for " pattern)
	      (let* ((a-keys (pgdb:ordered-data->a-keys ordered-data))
		   (b-keys (pgdb:ordered-data->b-keys ordered-data a-keys)))
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
				     (let* ((total (vector-ref val 2))
                                            (event-time (vector-ref val 1)) 
					    (pass  (vector-ref val 3))
					    (fail  (vector-ref val 4))
					    (other (vector-ref val 5))
                                            (id (vector-ref val 6)) 
					    (passper (round (* (/ pass total) 100)))
					    (failper (- 100 passper))
                                             (history (pgdb:get-run-stats-history-given-target dbh 1 (conc col-key "/" row-key)))  
         				     (history-hash (pgdb:get-history-hash history))
                                             (history-keys (sort (hash-table-keys history-hash) string>=?))
					    (run-key (string-substitute "[/]" "_x_" (conc col-key "/" row-key) 'all)))
				       (s:td   'style (conc "background: -webkit-linear-gradient(left, green " passper "%, red); background: -o-linear-gradient(right, green " passper "%, red); background: -moz-linear-gradient(right, green " passper "%, red); background: linear-gradient(to right, green " passper "%, red);")
      				      (s:a 'class "white"  'href (s:link-to "run" 'target run-key)
					  (conc "Latest:" total "/" pass "/" fail "/" other)) (s:span "  | ") (s:a 'id id 'class "viewmodal"  'title "Click to see description"  "History") (s:br)
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
                                                              history-keys)))))))
				     (s:td ""))))
			     a-keys)))
		    b-keys))))
)
(begin 
(s:p ""))))))
 *process*))))
