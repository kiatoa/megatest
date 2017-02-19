
(use test)

;;                       S  M  H  MD MTH  YR WD 
(define ref-time (vector 58 39 21 18 1   117 6  48 #f 25200))

(for-each
 (lambda (situation crontab ref-seconds last-done expected)
   (print "\nsituation: " situation)
   (print "ref-seconds: " ref-seconds " = " (time->string (seconds->local-time ref-seconds)))
   (print "last-done:   " last-done   " = " (time->string (seconds->local-time last-done)))
   (print "crontab:     " crontab)
   (test #f expected (common:cron-event crontab ref-seconds last-done)))
 '("midnight"   "midnight, already done" "diffdate"    "diffdate, already done" "diffday"    "sameday, already done") 
 '("0 0 * * *"  "0 0 * * *"              "0 0 18 * *" "0 0 18 * *"              "0 0 * * 5" "0 0 18 * 6"            )
 '(1487489998.0 1487489998.0             1487489998.0 1487489998.0              1487489998.0 1487489998.0            )
 '(1487479198.0 1487489098.0             1487479198.0 1487489098.0              1487479198.0 1487489098.0            )
 '(     #t           #f                       #f           #f                        #f           #f                 )
 )
