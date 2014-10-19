;;======================================================================
;; S E R V E R
;;======================================================================

;; Run like this:
;;
;;  (cd ..;make && make install) && ./rununittest.sh server 1;(cd simplerun;megatest -stop-server 0)

(test #f #t                 (vector? (make-dbr:dbstruct "/tmp")))

(define dbstruct (make-dbr:dbstruct "/tmp"))

(test #f #t                 (begin (dbr:dbstruct-set-main! dbstruct "blah") #t))
(test #f "blah"             (dbr:dbstruct-get-main  dbstruct))
(for-each 
 (lambda (run-id)
   (test #f #t                 (vector? (dbr:dbstruct-get-rundb-rec dbstruct run-id))))
 (list 1 2 3 4 5 6 7 8 9 #f))

(test #f 0 (dbr:dbstruct-field-name->num 'rundb))
(test #f 1 (dbr:dbstruct-field-name->num 'inmem))
(test #f 2 (dbr:dbstruct-field-name->num 'mtime))

(test #f #f (dbr:dbstruct-get-runvec-val dbstruct 1 'rundb))
(test #f #t (begin (dbr:dbstruct-set-runvec-val! dbstruct 1 'rundb "rundb") #t))
(test #f "rundb" (dbr:dbstruct-get-runvec-val dbstruct 1 'rundb))

(for-each
 (lambda (k)
   (test #f #t                 (begin (dbr:dbstruct-set-runvec-val! dbstruct 1 k (conc k)) #t))
   (test #f (conc k)           (dbr:dbstruct-get-runvec-val dbstruct 1 k)))
 '(rundb inmem mtime rtime stime inuse))

