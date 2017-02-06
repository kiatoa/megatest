
(declare (unit diff-report))
(declare (uses common))
(declare (uses rmt))
         
(include "common_records.scm")
(use matchable)
(use fmt)
(use ducttape-lib)
(define css "")

(define (diff:tests-mindat->hash tests-mindat)
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
(define (diff:status-compare3 status1 status2)
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


(define (diff:xor-tests-mindat src-tests-mindat dest-tests-mindat #!key (hide-clean #f) (consistent-fail-not-clean #f))
  (let* ((src-hash (diff:tests-mindat->hash src-tests-mindat))
         (dest-hash (diff:tests-mindat->hash dest-tests-mindat))
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

                     (dest-value    (hash-table-ref/default dest-hash key (list 0 "NULL" "NULL"))) ;; (list test-id state status)
                     (dest-test-id  (list-ref dest-value 0))
                     (dest-state    (list-ref dest-value 1))
                     (dest-status   (list-ref dest-value 2))

                     (src-value     (hash-table-ref/default src-hash key (list 0 "NULL" "NULL")))   ;; (list test-id state status)
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
                     (status-compare-result (diff:status-compare3 src-status dest-status))
                     (xor-new-item
                      (cond
                       ;; complete, for this case means: state=compelte AND status not in ( deleted uncomplete stuck/dead n/a )
                       ;; neither complete -> bad

                       ;; src !complete, dest complete -> better
                       ((and (not dest-complete) (not src-complete))
                        (list dest-test-id "BOTH-BAD" "BOTH-INCOMPLETE") src-value dest-value)
                       ((not dest-complete)
                        (list src-test-id "NOT-IN-DEST" "DEST-INCOMPLETE") src-value dest-value)  
                       ((not src-complete)
                        (list dest-test-id "NOT-IN-SRC" "SRC-INCOMPLETE") src-value dest-value)      
                       ((and
                         (equal? src-state dest-state)
                         (equal? src-status dest-status))
                        (if (and consistent-fail-not-clean (not (member dest-status '("PASS" "SKIP" "WAIVED" "WARN"))))
                            (list dest-test-id  (conc "BOTH-BAD") (conc "CLEAN-" dest-status) src-value dest-value)
                            (list dest-test-id  (conc "CLEAN") (conc "CLEAN-" dest-status) src-value dest-value)))
                       ;;    better or worse: pass > warn > waived > skip > fail > abort
                       ;;     pass > warn > waived > skip > fail > abort
                       
                       ((= 1 status-compare-result) ;; src is better, dest is worse
                        (list dest-test-id "WORSE" (conc src-status "->" dest-status) src-value dest-value))
                       (else
                        (list dest-test-id "BETTER" (conc src-status "->" dest-status) src-value dest-value)))))
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

(define (diff:run-name->run-id area-dat run-name)
  (if (number? run-name)
      run-name
      (let* ((qry-res (rmt:get-runs area-dat run-name 1 0 '())))
        (if (eq? 2 (vector-length qry-res))
            (vector-ref (car (vector-ref qry-res 1)) 1)
            #f))))

(define (diff:target+run-name->run-id area-dat target run-name)
  (let* ((keys (rmt:get-keys area-dat))
         (target-parts (if target (string-split target "/") (map (lambda (x) "%") keys))))
    (if (not (eq? (length keys) (length keys)))
        (begin
          (print "Error: Target ("target") item count does not match fields count target tokens="target-parts" fields="keys)
          #f)
        (let* ((target-map (zip keys target-parts))
               (qry-res (rmt:get-runs area-dat run-name 1 0 target-map)))

          (if (eq? 2 (vector-length qry-res))
              (let ((first-ent (vector-ref qry-res 1)))
                (if (> (length first-ent) 0)
                    (vector-ref (car first-ent) 1)
                    #f))
              #f)))))

(define (diff:run-id->tests-mindat area-dat run-id #!key (testpatt "%/%"))
  (let* ((states '())
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
     
     (rmt:get-tests-for-run area-dat run-id
                            testpatt states statuses
                            offset limit
                            not-in sort-by sort-order
                            qryvals
                            last-update
                            mode))))


(define (diff:diff-runs src-run-id dest-run-id)
  (let* ((src-tests-mindat  (diff:run-id->tests-mindat src-run-id))
         (dest-tests-mindat (diff:run-id->tests-mindat dest-run-id)))
    (diff:xor-tests-mindat src-tests-mindat dest-tests-mindat consistent-fail-not-clean: #t)))


(define (diff:rundiff-find-by-state run-diff state)
    (filter
     (lambda (x)
       (equal? (list-ref (caddr x) 1) state))
     run-diff))

(define (diff:rundiff-clean-breakdown run-diff)
  (map
   (lambda (run-diff-item)
     (match run-diff-item
       ((test-name item-path (junk-id diff-state diff-status (src-test-id src-state src-status) (dest-test-id dest-state dest-status)))
        (list test-name item-path "CLEAN" src-status))
       (else "")))
   (diff:rundiff-find-by-state run-diff "CLEAN")))
  
(define (diff:summarize-run-diff run-diff)
  
  (let* ((diff-states (list "CLEAN" "BETTER" "WORSE" "BOTH-BAD" "NOT-IN-DEST" "NOT-IN-SRC" )))
    (map
     (lambda (state)
       (list state 
             (length (diff:rundiff-find-by-state run-diff state))))
     diff-states)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Presentation code below, business logic above ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (diff:stml->string in-stml)
  (with-output-to-string
    (lambda ()
      (s:output-new
       (current-output-port)
       in-stml))))

(define (diff:state-status->bgcolor state status)
  (match (list state status)
    (("CLEAN"           _) "#88ff88")
    (("BETTER"          _) "#33ff33")
    (("WORSE"           _) "#ff3333")
    (("BOTH-BAD"        _) "#ff3333")
    ((_            "WARN") "#ffff88")
    ((_            "FAIL") "#ff8888")
    ((_           "ABORT") "#ff0000")
    ((_            "PASS") "#88ff88")
    ((_            "SKIP") "#ffff00")           
    (else                  "#ffffff")))

(define (diff:test-state-status->diff-report-cell state status)
  (s:td 'bgcolor (diff:state-status->bgcolor state status) status))

(define (diff:diff-state-status->diff-report-cell state status)
  (s:td state 'bgcolor (diff:state-status->bgcolor state status)))


(define (diff:megatest-html-logo)

  "<pre>
___  ___                 _            _
|  \\/  | ___  __ _  __ _| |_ ___  ___| |_
| |\\/| |/ _ \\/ _` |/ _` | __/ _ \\/ __| __|
| |  | |  __/ (_| | (_| | ||  __/\\__ \\ |_
|_|  |_|\\___|\\__, |\\__,_|\\__\\___||___/\\__|
             |___/
</pre>")

(define (diff:megatest-html-diff-logo)
  "<pre>
___  ___                 _            _
|  \\/  | ___  __ _  __ _| |_ ___  ___| |_  |  _ \\(_)/ _|/ _|
| |\\/| |/ _ \\/ _` |/ _` | __/ _ \\/ __| __| | | | | | |_| |_
| |  | |  __/ (_| | (_| | ||  __/\\__ \\ |_  | |_| | |  _|  _|
|_|  |_|\\___|\\__, |\\__,_|\\__\\___||___/\\__| |____/|_|_| |_|
             |___/
</pre>")


(define (diff:run-id->target+run-name+starttime area-dat run-id)
  (let* ((target      (rmt:get-target area-dat run-id))
         (runinfo     (rmt:get-run-info area-dat run-id)) ; vector of header (list) and result (vector)
         (info-hash   (alist->hash-table
                       (map (lambda (x) (cons (car x) (cadr x)))  ; make it a useful hash
                            (zip (vector-ref runinfo 0) (vector->list (vector-ref runinfo 1))))))
         (run-name    (hash-table-ref/default info-hash "runname" "N/A"))
         (start-time  (hash-table-ref/default info-hash "event_time" 0)))
    (list target run-name start-time)))

(define (diff:deliver-diff-report src-run-id dest-run-id
                                    #!key
                                    (html-output-file #f)
                                    (email-subject-prefix "[MEGATEST DIFF]")
                                    (email-recipients-list '())  )
  (let* ((src-info         (diff:run-id->target+run-name+starttime src-run-id))
         (src-target       (car src-info))
         (src-run-name     (cadr src-info))
         (src-start        (conc (seconds->string (caddr src-info)) " " (local-timezone-abbreviation)))
         (dest-info        (diff:run-id->target+run-name+starttime dest-run-id))
         (dest-target      (car dest-info))
         (dest-run-name    (cadr dest-info))
         (dest-start       (conc (seconds->string (caddr dest-info)) " " (local-timezone-abbreviation)))


         (run-diff (diff:diff-runs src-run-id dest-run-id ))
         (test-count (length run-diff))
         (summary-table
          (apply s:table 'cellspacing "0" 'border "1"
                 (s:tr
                  (s:th "Diff type")
                  (s:th "% share")
                  (s:th "Count"))
                 
                 (map
                  (lambda (state-count)
                    (s:tr
                     (diff:diff-state-status->diff-report-cell (car state-count) #f)
                     (s:td 'align "right" (fmt #f
                                (decimal-align 3
                                               (fix 2
                                                    (num/fit 6
                                                             (* 100 (/ (cadr state-count) test-count)))))))
                     (s:td 'align "right" (cadr state-count))))
                  (diff:summarize-run-diff run-diff))))
         (meta-table
          (s:table 'cellspacing "0" 'border "1"
                   
           (s:tr
            (s:td 'colspan "2"
                  (s:table 'cellspacing "0" 'border "1"
                           (s:tr
                            (s:th 'align "LEFT" "")          (s:th "SOURCE RUN")     (s:th "DESTINATION RUN"))
                           (s:tr
                            (s:th 'align "LEFT" "Started")  (s:td src-start)  (s:td dest-start))
                           (s:tr
                            (s:th 'align "LEFT" "TARGET")  (s:td src-target)  (s:td dest-target))
                           (s:tr
                            (s:th 'align "LEFT" "RUN NAME")  (s:td src-run-name)  (s:td dest-run-name)))))))
           
         (main-table
          (apply s:table 'cellspacing "0" 'border "1"
                 (s:tr
                  (s:th "Test name")
                  (s:th "Item Path")
                  (s:th (conc "SOURCE"))
                  (s:th (conc "DEST"))
                  (s:th "Diff"))
                 (map
                  (lambda (run-diff-item)
                    (match run-diff-item
                      ((test-name item-path (junk-id diff-state diff-status (src-test-id src-state src-status) (dest-test-id dest-state dest-status)))
                       (s:tr
                        (s:td test-name)
                        (s:td item-path)
                        (diff:test-state-status->diff-report-cell src-state src-status)
                        (diff:test-state-status->diff-report-cell dest-state dest-status)
                        (diff:diff-state-status->diff-report-cell diff-state diff-status)))
                      (else "")))
                  (filter (lambda (run-diff-item)
                            (match run-diff-item
                              ((test-name item-path (junk-id diff-state diff-status (src-test-id src-state src-status) (dest-test-id dest-state dest-status)))
                               (not (equal? diff-state "CLEAN")))
                              (else #f)))
                            run-diff))))
         (email-subject (conc email-subject-prefix " " src-target "/" src-run-name" vs. "dest-target"/"dest-run-name))
         (html-body     (diff:stml->string (s:body
                   (diff:megatest-html-diff-logo)
                   (s:h2 "Summary")
                   (s:table 'border "0"
                            (s:tr
                             (s:td "Diff calculated at")
                             (s:td (conc (seconds->string) " " (local-timezone-abbreviation))))
                            (s:tr
                             (s:td "MT_RUN_AREA_HOME" ) (s:td *toppath*))
                            (s:tr 'valign "TOP"
                     (s:td summary-table)
                     (s:td meta-table)))
                   (s:h2 "Diffs + consistently failing tests")
                   main-table)))

         )
    (if html-output-file
        (with-output-to-file html-output-file (lambda () (print html-body))))
    (when (and email-recipients-list (> (length email-recipients-list) 0))
      (sendmail (string-join email-recipients-list ",") email-subject html-body use_html: #t))
    html-body))
      

  


;; (let* ((src-run-name "all57")
;;        (dest-run-name "all60")
;;        (src-run-id (diff:run-name->run-id src-run-name))
;;        (dest-run-id (diff:run-name->run-id dest-run-name))
;;        (to-list (list "bjbarcla")))
;;   (diff:deliver-diff-report src-run-id dest-run-id email-recipients-list: to-list html-output-file: "/tmp/bjbarcla/zippy.html")
;;   )

(define (do-diff-report src-target src-runname dest-target dest-runname html-file to-list-raw)
  (let* (;;(src-target "nope%")
         ;;(src-runname "all57")
         ;;(dest-target "%")
         ;;(dest-runname "all60")
         (src-run-id (diff:target+run-name->run-id src-target src-runname))
         (dest-run-id (diff:target+run-name->run-id dest-target dest-runname))
         ;(html-file "/tmp/bjbarcla/zippy.html")
         (to-list (if (string? to-list-raw) (string-split to-list-raw ",:") #f))
         )
    
    (cond
     ((not src-run-id)
      (print "No match for source target/runname="src-target"/"src-runname)
      (print "Cannot proceed.")
      #f)
     ((not dest-run-id)
      (print "No match for source target/runname="dest-target"/"dest-runname)
      (print "Cannot proceed.")
      #f)
     (else
      (diff:deliver-diff-report src-run-id dest-run-id email-recipients-list: to-list html-output-file: html-file)))))

  
