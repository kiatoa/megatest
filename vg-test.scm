(use canvas-draw iup)
(import canvas-draw-iup)

(load "vg.scm")

(use trace)
(trace 
 vg:draw-rect
 vg:grow-rect
 vg:components-get-extents)

(define d1 (vg:drawing-new))
(define l1 (vg:lib-new))
(define c1 (vg:comp-new))
(define c2 (vg:comp-new))
(let ((r1 (vg:make-rect 20 20 40 40 text: "r1" font: "Helvetica, -20"))
      (r2 (vg:make-rect 40 40 80 80 text: "r2" font: "Helvetica, -10"))
      (t1 (vg:make-text 40 40 "The middle" font: "Helvetica, -10")))
  (vg:add-objs-to-comp c1 r1 r2 t1))

;; add the c1 component to lib l1 with name firstcomp
(vg:add-comp-to-lib l1 "firstcomp" c1)
(vg:add-comp-to-lib l1 "secondcomp" c2)

;; add the l1 lib to drawing with name firstlib
(vg:add-lib d1 "firstlib" l1)

;; instantiate firstlib/firstcomp as inst1 in drawing d1 at 0,0
(vg:instantiate d1 "firstlib" "firstcomp" "inst1" 0 0)
(vg:instantiate d1 "firstlib" "firstcomp" "inst2" 200 200)

;; (vg:drawing-scalex-set! d1 1.1)
;; (vg:drawing-scaley-set! d1 0.5)

;; (define xtnts (vg:scale-offset-xy 
;; 	       (vg:component-get-extents c1)
;; 	       1.1 1.1 -2 -2))

;; get extents of c1 and put a rectange around it
;;
(define xtnts (apply vg:grow-rect 10 10 (vg:components-get-extents d1 c1)))
(vg:add-objs-to-comp c1 (apply vg:make-rect xtnts))

;; get extents of all objects and put rectangle around it
;;
(define big-xtnts (vg:instances-get-extents d1))
(vg:add-objs-to-comp c2 (apply vg:make-rect big-xtnts))
(vg:instantiate d1 "firstlib" "secondcomp" "inst3" 0 0)

(vg:drawing-scalex-set! d1 1.5)
(vg:drawing-scaley-set! d1 1.5)

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
(vg:draw d1 #t)

;; (canvas-rectangle! cnv  10 100 10 80)

(main-loop)
