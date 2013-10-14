
(use sql-de-lite)
(include "test-common.scm")

(define db (open-database "test.db"))

(exec (sql db test-table-defn))
(exec (sql db syncsetup))

(define (register-test stmth  run-id testname host cpuload diskfree uname rundir shortdir item-path state status final-logf run-duration comment event-time)
    (exec 
     stmth ;; (sql db test-insert)
     run-id
     testname host cpuload diskfree uname rundir shortdir item-path state status final-logf run-duration comment event-time))

(let ((stmth (sql db test-insert)))
  (create-tests stmth))

(close-database db)