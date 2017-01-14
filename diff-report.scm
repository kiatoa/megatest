;; #!/bin/bash

;; #;; rmt:get-tests-for-run


;; #;; (let* ((dbstruct        (db:get-db

        
;; #;; (db:get-tests-for-run dbstruct run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals last-update mode)

;; #;; (rmt:get-test-info-by-id run-id test-id)
;; #;;  (rmt:get-tests-for-run run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals last-update mode)

;; megatest -repl << EOF

;; TODO:dashboard not on homehost message exit

(define (run-name->run-id runname)
  (let* ((qry-res (rmt:get-runs runname 1 0 '())))
    (if (eq? 2 (vector-length qry-res))
        (vector-ref (car (vector-ref qry-res 1)) 1)
        #f)))


(define (run-name->test-ht runname)
  (let* ((ht (make-hash-table))
         (run-id (run-name->run-id runname))
         (testpatt "%/%")
         (states '("COMPLETED" "INCOMPLETE"))
         (statuses '("PASS" "FAIL" "ABORT" "SKIP"))
         (offset #f)
         (limit #f)
         (not-in #f)
         (sort-by #f)
         (sort-order #f)
         (qryvals "id,testname,item_path,state,status")
         (last-update 0)
         (mode #f)
         )
    (print run-id)
    (print (rmt:get-tests-for-run run-id
                                  testpatt states statuses
                                  offset limit
                                  not-in sort-by sort-order
                                  qryvals
                                  last-update
                                  mode))
    ;(print (rmt:get-tests-for-run run-id testpatt  states statuses offset limit not-in "%" "%" #f "id,testname,testpath,state,status" 0 'normal))
    ;(print run-id)
    ))

(run-name->test-ht "all57")






;;(exit)

;;EOF

         
