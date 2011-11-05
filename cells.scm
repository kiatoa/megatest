;; (require-library iup canvas-draw)
;; 
;; (module cells-test
;; 	(cells-dialog)
;; 	(import
;; 	 scheme chicken extras
;; 	 iup canvas-draw
;; 	 (only canvas-draw-base pointer->canvas))
;; 

(use iup canvas-draw canvas-draw-base)
(define ncols  8)
(define nlins  8)
(define width  32)
(define height  32)

(define (render-cell handle i j x-min x-max y-min y-max canvas)
  (set! (canvas-foreground canvas)
	(if (or (and (odd? i) (odd? j)) (and (even? i) (even? j)))
	    #xffffff
	    #x000000))
  (canvas-box! canvas x-min x-max y-min y-max))

(define cells-dialog
  (dialog
   #:title "Cells Test"
   (cells
    #:rastersize (format "~sx~s" (* ncols width) (* nlins height))
    #:ncols-cb (lambda _ ncols) #:width-cb (lambda _ width)
    #:nlines-cb (lambda _ nlins) #:height-cb (lambda _ height)
    #:draw-cb
    ;; (make-cells-draw-cb render-cell))))

    (let ([wrap (pointer->canvas #t)])
      (lambda (handle i j x-min x-max y-min y-max canvas)
	(render-cell handle i j x-min x-max y-min y-max (wrap canvas)))))))

;; )

;; (import
;;  (only iup show main-loop)
;;  cells-test)

(show cells-dialog)
(main-loop)
