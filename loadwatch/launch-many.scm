(use posix)

(let loop ((count 0))
  (if (> count 500000)
      (print "DONE")
      (let ((cmd (conc "./queuefeeder xena:22022 bsub ./testopenlava.sh " count " " (random 30))))
	(print "Running: " cmd)
	(system cmd)
	(loop (+ count 1)))))
