;; #!/bin/bash

;; #;; rmt:get-tests-for-run


;; #;; (let* ((dbstruct        (db:get-db

        
;; #;; (db:get-tests-for-run dbstruct run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals last-update mode)

;; #;; (rmt:get-test-info-by-id run-id test-id)
;; #;;  (rmt:get-tests-for-run run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals last-update mode)

;; megatest -repl << EOF

;; TODO:dashboard not on homehost message exit

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

                     (dest-value (hash-table-ref/default dest-hash key #f)) ;; (list test-id state status)
                     (dest-test-id  (if dest-value (list-ref dest-value 0) #f))
                     (dest-state    (if dest-value (list-ref dest-value 1) #f))
                     (dest-status   (if dest-value (list-ref dest-value 2) #f))

                     (src-value     (hash-table-ref/default src-hash key #f))   ;; (list test-id state status)
                     (src-test-id   (if src-value (list-ref src-value 0) #f))
                     (src-state     (if src-value (list-ref src-value 1) #f))
                     (src-status    (if src-value (list-ref src-value 2) #f))

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
                        (list dest-test-id "BOTH-BAD" "BOTH-INCOMPLETE"))
                       ((not dest-complete)
                        (list src-test-id "DIFF-MISSING" "DEST-INCOMPLETE"))  
                       ((not src-complete)
                        (list dest-test-id "DIFF-NEW" "SRC-INCOMPLETE"))      
                       ((and
                         (equal? src-state dest-state)
                         (equal? src-status dest-status))
                        (list dest-test-id  (conc "CLEAN") (conc "CLEAN-" dest-status) )) 
                       ;;    better or worse: pass > warn > waived > skip > fail > abort
                       ;;     pass > warn > waived > skip > fail > abort
                       
                       ((= 1 status-compare-result) ;; src is better, dest is worse
                        (list dest-test-id "DIRTY-WORSE" (conc src-status "->" dest-status)))
                       (else
                        (list dest-test-id "DIRTY-BETTER" (conc src-status "->" dest-status)))
                       )))
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
    (for-each
     (lambda (state)
       (print "state="state "; "
              (length (rundiff-find-by-state run-diff state))))
     diff-states)))

     
(let* ((run-diff (diff-runs "all57" "all60"))
       (diff-summary (summarize-run-diff run-diff)))
  (pretty-print diff-summary))

         
;;  (match de (  (test-name test-path ( test-id "BOTH-BAD" test-status))   test-path) (else #f))
