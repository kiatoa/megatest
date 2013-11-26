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
(test #f #t                 (vector? (dbr:dbstruct-get-rundb-rec dbstruct 1)))

(for-each
 (lambda (k)
   (test #f #t                 (begin (dbr:dbstruct-set-runvec! dbstruct 1 k (conc k)) #t))
   (test #f k                  (dbr:dbstruct-get-runvec dbstruct 1 k)))
 '(rundb inmem mtime rtime stime inuse))