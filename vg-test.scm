(use canvas-draw iup)
(import canvas-draw-iup)

(load "vg.scm")

(define d1 (vg:drawing-new))
(define l1 (vg:lib-new))
(define c1 (vg:comp-new))
(let ((r1 (vg:make-rect 10 10 100 80))
      (r2 (vg:make-rect 100 80 190 150)))
  (vg:add-objs-to-comp c1 r1 r2))

;; add the c1 component to lib l1 with name firstcomp
(vg:add-comp-to-lib l1 "firstcomp" c1)

;; add the l1 lib to drawing with name firstlib
(vg:add-lib d1 "firstlib" l1)

;; instantiate firstlib/firstcomp as inst1 in drawing d1 at 0,0
(vg:instantiate d1 "firstlib" "firstcomp" "inst1" 0 0 0)
(vg:instantiate d1 "firstlib" "firstcomp" "inst2" 200 200 0)

(define cnv #f)
(define the-cnv (canvas 
		 #:size "500x400"
		 #:expand "YES"
		 #:scrollbar "YES"
		 #:posx "0.5"
		 #:posy "0.5"
		 #:action (make-canvas-action
			   (lambda (c xadj yadj)
			     (set! cnv c)))))

(show
 (dialog
  (vbox
   the-cnv)))

(vg:drawing-cnv-set! d1 cnv)
(vg:draw d1)

;; (canvas-rectangle! cnv  10 100 10 80)

(main-loop)
