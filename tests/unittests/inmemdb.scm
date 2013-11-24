;;======================================================================
;; S E R V E R
;;======================================================================

;; Run like this:
;;
;;  (cd ..;make && make install) && ./rununittest.sh server 1;(cd simplerun;megatest -stop-server 0)

(set! *transport-type* 'http)

(system "cp ../fullrun/megatest.db megatest.db")

(test "open inmem db"          1  (begin (open-in-mem-db) 1))

(test "setup for run" #t (begin (setup-for-run)
				(string? (getenv "MT_RUN_AREA_HOME"))))

(system "megatest -server - -debug 0 &")

(thread-sleep! 3) ;; need to wait for server to start. Yes, a better way is needed.

(define *keys*               (keys:config-get-fields *configdat*))
(define *keyvals*            (keys:target->keyval *keys* "a/b/c"))

(test #f #t                       (string? (car *runremote*)))
(test #f '(#t "successful login") (rmt:login)) ;;  *runremote* *toppath* *my-client-signature*)))

(define inmem (open-in-mem-db))

(define (inmem-test t b)
  (test "inmem sync to"   t (db:sync-to *db* inmem))
  (test "inmem sync back" b (db:sync-to inmem *db*)))

(inmem-test 0 0)

(inmem-test 1 1)

;;======================================================================
;; D B
;;======================================================================

(test #f '(#t "exit process started") (rmt:kill-server)) ;; *toppath* *my-client-signature* #f)))

 