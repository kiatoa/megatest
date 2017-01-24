;; #!/bin/bash

;; #;; rmt:get-tests-for-run


;; #;; (let* ((dbstruct        (db:get-db

        
;; #;; (db:get-tests-for-run dbstruct run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals last-update mode)

;; #;; (rmt:get-test-info-by-id run-id test-id)
;; #;;  (rmt:get-tests-for-run run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals last-update mode)

;; megatest -repl << EOF

;; TODO:dashboard not on homehost message exit

(use matchable)
(use ducttape-lib)
(define css "")

(define (tests-mindat->hash tests-mindat)
  (let* ((res (make-hash-table)))
    (for-each
     (lambda (item)
       (let* ((test-name+item-path (cons (list-ref item 0) (list-ref item 1)))
              (value (list-ref item 2)))
         (hash-table-set! res test-name+item-path value)))
     tests-mindat)
    res))

;; return 1 if status1 is better
;; return 0 if status1 and 2 are equally good
;; return -1 if status2 is better
(define (status-compare3 status1 status2)
  (let*
      ((status-goodness-ranking  (list "PASS" "WARN" "WAIVED" "SKIP" "FAIL" "ABORT" #f))
       (mem1 (member status1 status-goodness-ranking))
       (mem2 (member status2 status-goodness-ranking))
       )
    (cond
     ((and (not mem1) (not mem2)) 0)
     ((not mem1) -1)
     ((not mem2) 1)
     ((= (length mem1) (length mem2)) 0)
     ((> (length mem1) (length mem2)) 1)
     (else -1))))


(define (xor-tests-mindat src-tests-mindat dest-tests-mindat #!key (hide-clean #f))
  (let* ((src-hash (tests-mindat->hash src-tests-mindat))
         (dest-hash (tests-mindat->hash dest-tests-mindat))
         (all-keys
          (reverse (sort 
           (delete-duplicates
            (append (hash-table-keys src-hash) (hash-table-keys dest-hash)))

           (lambda (a b) 
             (cond
              ((< 0 (string-compare3 (car a) (car b))) #t)
              ((> 0 (string-compare3 (car a) (car b))) #f)
              ((< 0 (string-compare3 (cdr a) (cdr b))) #t)
              (else #f)))

           ))))
    (let ((res
           (map ;; TODO: rename xor to delta globally in dcommon and dashboard
            (lambda (key)
              (let* ((test-name (car key))
                     (item-path (cdr key))

                     (dest-value    (hash-table-ref/default dest-hash key (list #f #f #f))) ;; (list test-id state status)
                     (dest-test-id  (list-ref dest-value 0))
                     (dest-state    (list-ref dest-value 1))
                     (dest-status   (list-ref dest-value 2))

                     (src-value     (hash-table-ref/default src-hash key (list #f #f #f)))   ;; (list test-id state status)
                     (src-test-id   (list-ref src-value 0))
                     (src-state     (list-ref src-value 1))
                     (src-status    (list-ref src-value 2))

                     (incomplete-statuses '("DELETED" "INCOMPLETE" "STUCK/DEAD" "N/A")) ;; if any of these statuses apply, treat test as incomplete

                     (dest-complete
                      (and dest-value dest-state dest-status
                           (equal? dest-state "COMPLETED")
                           (not (member dest-status incomplete-statuses))))
                     (src-complete
                      (and src-value src-state src-status
                           (equal? src-state "COMPLETED")
                           (not (member src-status incomplete-statuses))))
                     (status-compare-result (status-compare3 src-status dest-status))
                     (xor-new-item
                      (cond
                       ;; complete, for this case means: state=compelte AND status not in ( deleted uncomplete stuck/dead n/a )
                       ;; neither complete -> bad

                       ;; src !complete, dest complete -> better
                       ((and (not dest-complete) (not src-complete))
                        (list dest-test-id "BOTH-BAD" "BOTH-INCOMPLETE") src-value dest-value)
                       ((not dest-complete)
                        (list src-test-id "DIFF-MISSING" "DEST-INCOMPLETE") src-value dest-value)  
                       ((not src-complete)
                        (list dest-test-id "DIFF-NEW" "SRC-INCOMPLETE") src-value dest-value)      
                       ((and
                         (equal? src-state dest-state)
                         (equal? src-status dest-status))
                        (list dest-test-id  (conc "CLEAN") (conc "CLEAN-" dest-status) src-value dest-value)) 
                       ;;    better or worse: pass > warn > waived > skip > fail > abort
                       ;;     pass > warn > waived > skip > fail > abort
                       
                       ((= 1 status-compare-result) ;; src is better, dest is worse
                        (list dest-test-id "DIRTY-WORSE" (conc src-status "->" dest-status) src-value dest-value))
                       (else
                        (list dest-test-id "DIRTY-BETTER" (conc src-status "->" dest-status) src-value dest-value)))))
                (list test-name item-path  xor-new-item)))
            all-keys)))

      (if hide-clean
          (filter
           (lambda (item)
             (not
              (equal?
               "CLEAN"
               (list-ref (list-ref item 2) 1))))
           res)
          res))))

(define (run-name->run-id runname)
  (if (number? runname)
      runname
      (let* ((qry-res (rmt:get-runs runname 1 0 '())))
        (if (eq? 2 (vector-length qry-res))
            (vector-ref (car (vector-ref qry-res 1)) 1)
            #f))))

(define (run-name->tests-mindat runname)
  (let* ((run-id (run-name->run-id runname))
         (testpatt "%/%")
;;         (states '("COMPLETED" "INCOMPLETE"))
 ;;        (statuses '("PASS" "FAIL" "ABORT" "SKIP"))
         (states '())
         (statuses '())
         (offset #f)
         (limit #f)
         (not-in #t)
         (sort-by #f)
         (sort-order #f)
         (qryvals "id,testname,item_path,state,status")
         (qryvals "id,testname,item_path,state,status")
         (last-update 0)
         (mode #f)
         )
    (map
     ;; (lambda (row)
     ;;   (match row
     ;;     ((#(id test-name item-path state status)
     ;;       (list test-name item-path (list id state status))))
     ;;     (else #f)))
     (lambda (row)
       (let* ((id        (vector-ref row 0))
              (test-name  (vector-ref row 1))
              (item-path (vector-ref row 2))
              (state     (vector-ref row 3))
              (status    (vector-ref row 4)))
             (list test-name item-path (list id state status))))
     
     (rmt:get-tests-for-run run-id
                            testpatt states statuses
                            offset limit
                            not-in sort-by sort-order
                            qryvals
                            last-update
                            mode))))


(define (diff-runs run1 run2)
  (let* ((src-tests-mindat  (run-name->tests-mindat run1))
         (dest-tests-mindat (run-name->tests-mindat run2)))
    (xor-tests-mindat src-tests-mindat dest-tests-mindat)));; #!key (hide-c


(define (rundiff-find-by-state run-diff state)
    (filter
     (lambda (x)
       (equal? (list-ref (caddr x) 1) state))
     run-diff))

  
(define (summarize-run-diff run-diff)
  (let* ((diff-states (list "CLEAN" "DIRTY-BETTER" "DIRTY-WORSE" "BOTH-BAD" "DIFF-MISSING" "DIFF-NEW" )))
    (map
     (lambda (state)
       (list state 
             (length (rundiff-find-by-state run-diff state))))
     diff-states)))

(define (stml->string in-stml)
  (with-output-to-string
    (lambda ()
      (s:output-new
       (current-output-port)
       in-stml))))

(define (test-state-status->diff-report-cell state status)
  (s:td status))

(define (diff-state-status->diff-report-cell state status)
  (s:td state 'bgcolor "#33ff33"))

(define (run-diff->diff-report src-runname dest-runname run-diff)
  (let* ((test-count (length run-diff))
         (summary-table
          (apply s:table 'cellspacing "0" 'border "1"
                 (s:tr
                  (s:th "Diff type")
                  (s:th "% share")
                  (s:th "Count"))
                 
                 (map
                  (lambda (state-count)
                    (s:tr
                     (s:td (car state-count))
                     (s:td (* 100 (/ (cadr state-count) test-count)))
                     (s:td (cadr state-count))))
                  (summarize-run-diff run-diff))))
         (main-table
          (apply s:table 'cellspacing "0" 'border "1"
                 (s:tr
                  (s:th "Test name")
                  (s:th "Item Path")
                  (s:th (conc "Source=" src-runname))
                  (s:th (conc "Dest=" dest-runname))
                  (s:th "Diff"))
                 (map
                  (lambda (run-diff-item)
                    (match run-diff-item
                      ((test-name item-path (junk-id diff-state diff-status (src-test-id src-state src-status) (dest-test-id dest-state dest-status)))
                       (s:tr
                        (s:td test-name)
                        (s:td item-path)
                        (test-state-status->diff-report-cell src-state src-status)
                        (test-state-status->diff-report-cell dest-state dest-status)
                        (diff-state-status->diff-report-cell diff-state diff-status)))
                      (else "")))
                  (filter (lambda (run-diff-item)
                            (match run-diff-item
                              ((test-name item-path (junk-id diff-state diff-status (src-test-id src-state src-status) (dest-test-id dest-state dest-status)))
                               (not (equal? diff-state "CLEAN")))
                              (else #f)))
                            run-diff)))))
    (stml->string (s:body
                   summary-table
                   main-table))))
    
                    
            
          
                               

(let* ((src-runname "all57")
       (dest-runname "all60")
       (to "bjbarcla")
       (subj (conc "[MEGATEST DIFF] "src-runname" vs. "dest-runname))
       (run-diff (diff-runs src-runname dest-runname))
       (diff-summary (summarize-run-diff run-diff))
       (html-report (run-diff->diff-report src-runname dest-runname run-diff)))
  ;;(pretty-print run-diff)
  ;;(pretty-print diff-summary)

  (sendmail to subj html-report use_html: #t)
  ;;(print html-report)
  )

         
;; (match de
;;   ((test-name test-path ( test-id "BOTH-BAD" test-status))   test-path)
;;   (else #f))
