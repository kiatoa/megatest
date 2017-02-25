
;; example of how to set up and write target mappers
;;
(define *target-mappers*
  `((prefix-contour . ,(lambda (target run-name area area-path reason contour mode-patt)
			 (conc contour "/" target)))))

;; (print "Yep, got here!")
