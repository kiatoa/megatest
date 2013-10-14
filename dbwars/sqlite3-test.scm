
(use sqlite3)
(include "test-common.scm")

(define db (open-database "test.db"))

(execute db test-table-defn)
(execute db syncsetup)


(define (register-test stmth run-id testname host cpuload diskfree uname rundir shortdir item-path state status final-logf run-duration comment event-time)
  (execute stmth
	   run-id
	   testname host cpuload diskfree uname rundir shortdir item-path state status final-logf run-duration comment event-time))

(let ((stmth (prepare db test-insert)))
  (create-tests stmth)
  (finalize! stmth))

(finalize! db)
