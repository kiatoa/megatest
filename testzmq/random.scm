(use posix)
(randomize (inexact->exact (current-seconds)))

(define low (string->number (cadr (argv))))
(define hi  (string->number (caddr (argv))))

(print (+ low (random (- hi low))))

