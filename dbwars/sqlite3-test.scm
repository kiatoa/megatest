
(use sqlite3)
(include "test-common.scm")

(define db (open-database "test.db"))

(execute db test-table-defn)

(define (register-test db run-id testname host cpuload diskfree uname rundir shortdir item-path state status final-logf run-duration comment event-time)
  (execute db
	   test-insert
	   run-id
	   testname host cpuload diskfree uname rundir shortdir item-path state status final-logf run-duration comment event-time))

