
(use sql-de-lite)
(include "test-common.scm")

(define db (open-database "test.db"))

(execute db (sql db test-table-defn))

(define (register-test db  run-id testname host cpuload diskfree uname rundir shortdir item-path state status final-logf run-duration comment event-time)
    (exec 
     (sql db test-insert)
     run-id
     testname host cpuload diskfree uname rundir shortdir item-path state status final-logf run-duration comment event-time))

