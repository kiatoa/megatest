(require-library iup canvas-draw canvas-draw-iup)

(module matrix-test
 	(matrix-dialog)
 	(import
 	 scheme chicken extras
 	 iup canvas-draw canvas-draw-iup
 	 (only canvas-draw-base pointer->canvas))
 
(define ncols  8)
(define nlins  8)
(define width  32)
(define height  32)

;; (define (render-cell handle i j x-min x-max y-min y-max canvas)
;;   (set! (canvas-foreground canvas)
;; 	(if (or (and (odd? i) (odd? j)) (and (even? i) (even? j)))
;; 	    #xffffff
;; 	    #x000000))
;;   (canvas-box! canvas x-min x-max y-min y-max))

(define matrix-dialog
  (dialog
   #:title "Matrix Test"
   (let ((mat (matrix
	       ; #:expand "YES"
	       ; #:scrollbar "YES"
	       #:numcol ncols 
	       #:numlin nlins 
	       #:numcol-visible ncols 
	       #:numlin-visible nlins
	       #:click-cb (lambda (obj lin col status)
			    (print "obj: " obj " lin: " lin " col: " col " status: " status)))))
     (attribute-set! mat "0:0" "Testing")
     mat)))

) ;; end module

(import
 (only iup show main-loop)
  matrix-test)

(show matrix-dialog)
(main-loop)
