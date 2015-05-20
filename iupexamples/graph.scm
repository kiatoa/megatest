(use iup)
(import iup-pplot)

 

(define (tl)
  (let* ((lastx 0)
	 (lastsample 2)
	 (plt (pplot 
	       #:title "MyTitle"
	       #:marginbottom "65"
	       #:marginleft   "65"
	       #:axs_xlabel   "Score" 
	       #:axs_ylabel "Count"
	       #:legendshow "YES"
	       ;; #:axs_xmin    "0"
	       ;; #:axs_ymin    "0"
	       #:axs_yautomin "YES"
	       #:axs_xautomin "YES"
	       #:axs_xautotick "YES"
	       #:axs_yautotick "YES"
	       #:ds_showvalues "YES"
	       #:size "200x200"
	       ))
	 (plt1  (call-with-pplot 
		 plt
		 (lambda (x)
		   (pplot-add! plt 10 100)
		   (pplot-add! plt 20 120)
		   (pplot-add! plt 30 200))
		 #:x-string #f
		 ))
	 (plt2  (call-with-pplot 
		 plt
		 (lambda (x)
		   (pplot-add! plt 10 180)
		   (pplot-add! plt 20 125)
		   (pplot-add! plt 30 100))
		 #:x-string #f
		 ))
	 (dlg (dialog
	       (vbox
		plt
		(hbox
		;;  (button "Redraw" size: "50x" action: (lambda (obj)
		;; 					(redraw plt)))
		 (button "Quit"   size: "50x" action: (lambda (obj)
							(exit)))
		 (button "AddPoint" size: "50x" action: (lambda (obj)
							  (set! lastx (+ lastx 10))
							  (set! lastsample (+ lastsample 1))
							  ;; (attribute-set! plt 'current 0)
							  (print "lastx: " lastx " lastsample: " lastsample)
							  (pplot-add! plt lastx (random 300) lastsample 1)
							  (attribute-set! plt "REDRAW" "1"))))))))
    (set! lastx 30)
    (attribute-set! plt 'ds_mode "LINE")
    ;; (attribute-set! plt 'ds_legend "Yada")
    (show dlg)
    (main-loop)))

(tl)