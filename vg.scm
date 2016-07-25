;;
;; Copyright 2016  Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

(use defstruct srfi-1)

(declare (unit vg))
(use canvas-draw iup)
(import canvas-draw-iup)

(include "vg_records.scm")

;; ;; structs
;; ;;
;; (defstruct vg:lib     comps)
;; (defstruct vg:comp    objs name file)
;; ;; extents caches extents calculated on draw
;; ;; proc is called on draw and takes the obj itself as a parameter
;; ;; attrib is an alist of parameters
;; (defstruct vg:obj     type pts fill-color text line-color call-back angle font attrib extents proc)
;; (defstruct vg:inst    libname compname theta xoff yoff scalex scaley mirrx mirry call-back cache)
;; (defstruct vg:drawing libs insts scalex scaley xoff yoff cnv cache) ;; libs: hash of name->lib, insts: hash of instname->inst

;; inits
;;
(define (vg:comp-new)
  (make-vg:comp objs: '() name: #f file: #f))

(define (vg:lib-new)
  (make-vg:lib comps: (make-hash-table)))

(define (vg:drawing-new)
  (make-vg:drawing scalex: 1 
		   scaley: 1 
		   xoff: 0 
		   yoff: 0 
		   libs: (make-hash-table) 
		   insts: (make-hash-table)
		   cache: '()))

;;======================================================================
;; scaling and offsets
;;======================================================================

(define-inline (vg:scale-offset val s o)
  (+ o (* val s)))
  ;; (* (+ o val) s))

;; apply scale and offset to a list of x y values
;;
(define (vg:scale-offset-xy lstxy sx sy ox oy)
  (if (> (length lstxy) 1) ;; have at least one xy pair
      (let loop ((x   (car lstxy))
		 (y   (cadr lstxy))
		 (tal (cddr lstxy))
		 (res '()))
	(let ((newres (cons (vg:scale-offset y sy oy)
			    (cons (vg:scale-offset x sx ox)
				  res))))
	  (if (> (length tal) 1)
	      (loop (car tal)(cadr tal)(cddr tal) newres)
	      (reverse newres))))
      '()))

;; apply drawing offset and scaling to the points in lstxy
;;
(define (vg:drawing-apply-scale drawing lstxy)
  (vg:scale-offset-xy 
   lstxy
   (vg:drawing-scalex drawing)
   (vg:drawing-scaley drawing)
   (vg:drawing-xoff   drawing)
   (vg:drawing-yoff   drawing)))

;; apply instance offset and scaling to the points in lstxy
;;
(define (vg:inst-apply-scale inst lstxy)
  (vg:scale-offset-xy 
   lstxy
   (vg:inst-scalex inst)
   (vg:inst-scaley inst)
   (vg:inst-xoff   inst)
   (vg:inst-yoff   inst)))

;; apply both drawing and instance scaling to a list of xy points
;; 
(define (vg:drawing-inst-apply-scale-offset drawing inst lstxy)
  (vg:drawing-apply-scale 
   drawing
   (vg:inst-apply-scale inst lstxy)))

;;======================================================================
;; objects
;;======================================================================

;;   (vg:inst-apply-scale 
;;    inst
;;    (vg:drawing-apply-scale drawing lstxy)))

;; make a rectangle obj
;;
(define (vg:make-rect-obj x1 y1 x2 y2 #!key (line-color #f)(fill-color #f)(text #f)(font #f)(extents #f))
  (make-vg:obj type: 'r pts: (list x1 y1 x2 y2) text: text font: font line-color: line-color fill-color: fill-color extents: extents))

;; make a rectangle obj
;; 
(define (vg:make-line-obj x1 y1 x2 y2 #!key (line-color #f)(fill-color #f)(text #f)(font #f)(extents #f))
  (make-vg:obj type: 'l pts: (list x1 y1 x2 y2) text: text font: font line-color: line-color extents: extents))

;; make a text obj
;;
(define (vg:make-text-obj x1 y1 text #!key (line-color #f)(fill-color #f)
		      (angle #f)(scale-with-zoom #f)(font #f)
		      (font-size #f))
  (make-vg:obj type: 't pts: (list x1 y1) text: text 
	       line-color: line-color fill-color: fill-color
	       angle: angle font: font extents: #f
	       attributes: (vg:make-attrib 'font-size font-size)))

;; proc takes startnum and endnum and yields scalef, per-grad and unitname
;;
(define (vg:make-xaxis-obj x1 y1 x2 y2 #!key (line-color #f)(fill-color #f)(text #f)(font #f)(proc #f))
  (make-vg:obj type: 'x pts: (list x1 y1 x2 y2) text: text font: font line-color: line-color fill-color: fill-color extents: #f proc: proc))

;;======================================================================
;; obj modifiers and queries
;;======================================================================

;; get extents, use knowledge of type ...
;;
(define (vg:obj-get-extents drawing obj)
  (let ((type (vg:obj-type obj)))
    (case type
      ((r)(vg:rect-get-extents obj))
      ((t)(vg:draw-text drawing obj draw: #f))
      (else #f))))

(define (vg:rect-get-extents obj)
  (vg:obj-pts obj)) ;; extents are just the points for a rectangle

(define (vg:grow-rect borderx bordery x1 y1 x2 y2)
  (list
   (- x1 borderx)
   (- y1 bordery)
   (+ x2 borderx)
   (+ y2 bordery)))

(define (vg:make-attrib . attrib-list)
  #f)

;;======================================================================
;; components
;;======================================================================

;; add obj to comp
;;
(define (vg:add-objs-to-comp comp . objs)
  (vg:comp-objs-set! comp (append (vg:comp-objs comp) objs)))

(define (vg:add-obj-to-comp comp obj)
  (vg:comp-objs-set! comp (cons obj (vg:comp-objs comp))))

;; use the struct. leave this here to remind of this!
;;
;; (define (vg:comp-get-objs comp)
;;   (vg:comp-objs comp))

;; add comp to lib
;;
(define (vg:add-comp-to-lib lib compname comp)
  (hash-table-set! (vg:lib-comps lib) compname comp))

;; instanciate component in drawing
;;
(define (vg:instantiate drawing libname compname instname xoff yoff #!key (theta 0)(scalex 1)(scaley 1)(mirrx #f)(mirry #f))
  (let ((inst (make-vg:inst libname: libname compname: compname xoff: xoff yoff: yoff theta: theta scalex: scalex scaley: scaley mirrx: mirrx mirry: mirry)) )
    (hash-table-set! (vg:drawing-insts drawing) instname inst)))

(define (vg:instance-move drawing instname newx newy)
  (let ((inst (hash-table-ref (vg:drawing-insts drawing) instname)))
    (vg:inst-xoff-set! inst newx)
    (vg:inst-yoff-set! inst newy)))

;; get component from drawing (look in apropriate lib) given libname and compname
(define (vg:get-component drawing libname compname)
  (let* ((lib  (hash-table-ref (vg:drawing-libs drawing) libname))
	 (inst (hash-table-ref (vg:lib-comps lib) compname)))
    inst))

(define (vg:get-extents-for-objs drawing objs)
  (if (or (not objs)
	  (null? objs))
      #f
      (let loop ((hed     (car objs))
		 (tal     (cdr objs))
		 (extents (vg:obj-get-extents drawing (car objs))))
	(let ((newextents
	       (vg:get-extents-for-two-rects
		extents
		(vg:obj-get-extents drawing hed))))
	  (if (null? tal)
	      extents
	      (loop (car tal)(cdr tal) newextents))))))

;;   (let ((extents #f))
;;     (for-each
;;      (lambda (obj)
;;        (set! extents
;; 	 (vg:get-extents-for-two-rects
;; 	  extents
;; 	  (vg:obj-get-extents drawing obj))))
;;      objs)
;;     extents))

;; given rectangles r1 and r2, return the box that bounds both
;;
(define (vg:get-extents-for-two-rects r1 r2)
  (if (not r1)
      r2
      (if (not r2)
	  r1 ;; #f ;; no extents from #f #f
	  (list (min (car r1)(car r2))           ;; llx
		(min (cadr r1)(cadr r2))         ;; lly
		(max (caddr r1)(caddr r2))       ;; ulx
		(max (cadddr r1)(cadddr r2)))))) ;; uly

(define (vg:components-get-extents drawing . comps)
  (if (null? comps)
      #f
      (let loop ((hed  (car comps))
		 (tal  (cdr comps))
		 (extents #f))
	(let* ((objs  (vg:comp-objs hed))
	       (newextents (if extents
			       (vg:get-extents-for-two-rects
				extents
				(vg:get-extents-for-objs drawing objs))
			       (vg:get-extents-for-objs drawing objs))))
	  (if (null? tal)
	      newextents
	      (loop (car tal)(cdr tal) newextents))))))

;;======================================================================
;; libraries
;;======================================================================

;; register lib with drawing

;;
(define (vg:add-lib drawing libname lib)
  (hash-table-set! (vg:drawing-libs drawing) libname lib))

(define (vg:get-lib drawing libname)
  (hash-table-ref/default (vg:drawing-libs drawing) libname #f))

(define (vg:get/create-lib drawing libname)
  (let ((lib (vg:get-lib drawing libname)))
    (if lib
	lib
	(let ((newlib (vg:lib-new)))
	  (vg:add-lib drawing libname newlib)
	  newlib))))

;;======================================================================
;; map objects given offset, scale and mirror, resulting obj is displayed
;;======================================================================

;; dispatch the drawing of obj off to the correct drawing routine
;;
(define (vg:map-obj drawing inst obj)
  (case (vg:obj-type obj)
    ((l)(vg:map-line   drawing inst obj))
    ((r)(vg:map-rect   drawing inst obj))
    ((t)(vg:map-text   drawing inst obj))
    ((x)(vg:map-xaxis  drawing inst obj))
    (else #f)))

;; given a drawing and a inst map a rectangle to it screen coordinates
;;
(define (vg:map-rect drawing inst obj)
  (let ((res (make-vg:obj type:       'r ;; is there a defstruct copy?
			  fill-color: (vg:obj-fill-color obj)
			  text:       (vg:obj-text       obj)
			  line-color: (vg:obj-line-color obj)
			  font:       (vg:obj-font       obj)))
	(pts (vg:obj-pts obj)))
    (vg:obj-pts-set! res (vg:drawing-inst-apply-scale-offset drawing inst pts))
    (vg:drawing-cache-set! drawing (cons res (vg:drawing-cache drawing) ))
    res))

;; given a drawing and a inst map a line to it screen coordinates
;;
(define (vg:map-line drawing inst obj)
  (let ((res (make-vg:obj type:       'l ;; is there a defstruct copy?
			  line-color: (vg:obj-line-color obj)
			  font:       (vg:obj-font       obj)))
	(pts (vg:obj-pts obj)))
    (vg:obj-pts-set! res (vg:drawing-inst-apply-scale-offset drawing inst pts))
    (vg:drawing-cache-set! drawing (cons res (vg:drawing-cache drawing) ))
    res))

;; given a drawing and a inst map a text to it screen coordinates
;;
(define (vg:map-text drawing inst obj)
  (let ((res (make-vg:obj type:       't
			  fill-color: (vg:obj-fill-color obj)
			  text:       (vg:obj-text       obj)
			  line-color: (vg:obj-line-color obj)
			  font:       (vg:obj-font       obj)
			  angle:      (vg:obj-angle      obj)
			  attrib:     (vg:obj-attrib     obj)))
	(pts (vg:obj-pts obj)))
    (vg:obj-pts-set! res (vg:drawing-inst-apply-scale-offset drawing inst pts))
    (vg:drawing-cache-set! drawing (cons res (vg:drawing-cache drawing)))
    res))

;; given a drawing and a inst map a line to it screen coordinates
;;
(define (vg:map-xaxis drawing inst obj)
  (let ((res (make-vg:obj type:      'x ;; is there a defstruct copy?
			  line-color: (vg:obj-line-color obj)
			  font:       (vg:obj-font       obj)))
	(pts (vg:obj-pts obj)))
    (vg:obj-pts-set! res (vg:drawing-inst-apply-scale-offset drawing inst pts))
    (vg:drawing-cache-set! drawing (cons res (vg:drawing-cache drawing) ))
    res))

;;======================================================================
;; instances
;;======================================================================

(define (vg:instances-get-extents drawing . instance-names)
  (let ((xtnt-lst (vg:draw drawing #f))
	(llx   #f)
	(lly   #f)
	(ulx   #f)
	(uly   #f))
    (for-each
     (lambda (extents)
       (let* ((ollx      (list-ref extents 0))
	      (olly      (list-ref extents 1))
	      (oulx      (list-ref extents 2))
	      (ouly      (list-ref extents 3)))
	 (if (or (not llx)(< ollx llx))(set! llx ollx))
	 (if (or (not lly)(< olly lly))(set! lly olly))
	 (if (or (not ulx)(> oulx ulx))(set! ulx oulx))
	 (if (or (not uly)(> ouly uly))(set! uly ouly))))
     xtnt-lst)
    (list llx lly ulx uly)))

(define (vg:lib-get-component lib instname)
  (hash-table-ref/default  (vg:lib-comps lib) instname #f))

;;======================================================================
;; color
;;======================================================================

(define (vg:rgb->number r g b #!key (a 0))
  (bitwise-ior
    (arithmetic-shift a 24)
    (arithmetic-shift r 16)
    (arithmetic-shift g 8)
    b))

(define (vg:iup-color->number iup-color)
  (apply vg:rgb->number (map string->number (string-split iup-color))))

;;======================================================================
;; graphing
;;======================================================================

(define (vg:make-xaxis drawing component x1 y1 x2 y2 startnum endnum scaleproc)
  (let ((obj (vg:make-xaxis-obj x1 y1 x2 y2)))
    #f))

;;======================================================================
;; Unravel and draw the objects
;;======================================================================

;; with get-extents = #t return the extents
;; with draw = #f don't actually draw the object
;;
(define (vg:draw-obj drawing obj #!key (draw #t))
  ;; (print "obj type: " (vg:obj-type obj))
  (case (vg:obj-type obj)
    ((r)(vg:draw-rect drawing obj draw: draw))
    ((t)(vg:draw-text drawing obj draw: draw))))

;; given a rect obj draw it on the canvas applying first the drawing
;; scale and offset
;;
(define (vg:draw-rect drawing obj #!key (draw #t))
  (let* ((cnv (vg:drawing-cnv drawing))
	 (pts (vg:drawing-apply-scale drawing (vg:obj-pts obj)))
	 (fill-color (vg:obj-fill-color obj))
	 (line-color (vg:obj-line-color obj))
	 (text       (vg:obj-text obj))
	 (font       (vg:obj-font obj))
	 (llx        (car pts))
	 (lly        (cadr pts))
	 (ulx        (caddr pts))
	 (uly        (cadddr pts))
	 (w          (- ulx llx))
	 (h          (- uly lly))
	 (text-xmax  #f)
	 (text-ymax  #f))
    (if draw 
	(let ((prev-background-color (canvas-background cnv))
	      (prev-foreground-color (canvas-foreground cnv)))
	  (if fill-color
	      (begin
		(canvas-foreground-set! cnv fill-color)
		(canvas-box! cnv llx ulx lly uly))) ;; docs are all over the place on this one.;; w h)
	  (if line-color
	      (canvas-foreground-set! cnv line-color)
	      (if fill-color
		  (canvas-foreground-set! cnv prev-foreground-color)))
	  (canvas-rectangle! cnv llx ulx lly uly)
	  (canvas-foreground-set! cnv prev-foreground-color)
	  (if text 
	      (let* ((prev-font    (canvas-font cnv))
		     (font-changed (and font (not (equal? font prev-font)))))
		(if font-changed (canvas-font-set! cnv font))
		(canvas-text! cnv (+ 2 llx)(+ 2 lly) text)
		(if (eq? draw 'get-extents)
		    (let-values (((xmax ymax)(canvas-text-size cnv text)))
				(set! text-xmax xmax)(set! text-ymax ymax)))
		(if font-changed (canvas-font-set! cnv prev-font))))))
    ;; (print "text-xmax: " text-xmax " text-ymax: " text-ymax)
    (if (vg:obj-extents obj)
	(vg:obj-extents obj)
	(if (not text)
	    pts ;; no text
	    (if (and text-xmax text-ymax) ;; have text
		(let ((xt (list llx lly
				(max ulx (+ llx text-xmax))
				(max uly (+ lly text-ymax)))))
		  (vg:obj-extents-set! obj xt)
		  xt)
		(if cnv
		    (if (eq? draw 'get-extents)
			(let-values (((xmax ymax)(canvas-text-size cnv text)))
				    (let ((xt (list llx lly
						    (max ulx (+ llx xmax))
						    (max uly (+ lly ymax)))))
				      (vg:obj-extents-set! obj xt)
				      xt))
			pts)
		    pts)))))) ;; return extents 

;; given a rect obj draw it on the canvas applying first the drawing
;; scale and offset
;;
(define (vg:draw-line drawing obj #!key (draw #t))
  (let* ((cnv (vg:drawing-cnv drawing))
	 (pts (vg:drawing-apply-scale drawing (vg:obj-pts obj)))
	 ;; (fill-color (vg:obj-fill-color obj))
	 (line-color (vg:obj-line-color obj))
	 (text       (vg:obj-text obj))
	 (font       (vg:obj-font obj))
	 (llx        (car pts))
	 (lly        (cadr pts))
	 (ulx        (caddr pts))
	 (uly        (cadddr pts))
	 (w          (- ulx llx))
	 (h          (- uly lly))
	 (text-xmax  #f)
	 (text-ymax  #f))
    (if draw 
	(let ((prev-background-color (canvas-background cnv))
	      (prev-foreground-color (canvas-foreground cnv)))
	;; (if fill-color
	;;     (begin
	;; 	(canvas-foreground-set! cnv fill-color)
	;; 	(canvas-box! cnv llx ulx lly uly))) ;; docs are all over the place on this one.;; w h)
	  (if line-color
	      (canvas-foreground-set! cnv line-color)
	      (if fill-color
		  (canvas-foreground-set! cnv prev-foreground-color)))
	  (canvas-line! cnv llx ulx lly uly)
	  (canvas-foreground-set! cnv prev-foreground-color)
	  (if text 
	      (let* ((prev-font    (canvas-font cnv))
		     (font-changed (and font (not (equal? font prev-font)))))
		(if font-changed (canvas-font-set! cnv font))
		(canvas-text! cnv (+ 2 llx)(+ 2 lly) text)
		(let-values (((xmax ymax)(canvas-text-size cnv text)))
		  (set! text-xmax xmax)(set! text-ymax ymax))
		(if font-changed (canvas-font-set! cnv prev-font))))))
    (print "text-xmax: " text-xmax " text-ymax: " text-ymax)
    (if (vg:obj-extents obj)
	(vg:obj-extents obj)
	(if (not text)
	    pts
	    (if (and text-xmax text-ymax)
		(let ((xt (list llx lly
				(max ulx (+ llx text-xmax))
				(max uly (+ lly text-ymax)))))
		  (vg:obj-extents-set! obj xt)
		  xt)
		(if cnv
		    (let-values (((xmax ymax)(canvas-text-size cnv text)))
		      (let ((xt (list llx lly
				      (max ulx (+ llx xmax))
				      (max uly (+ lly ymax)))))
			(vg:obj-extents-set! obj xt)
			xt))
		    pts)))))) ;; return extents 

;; given a rect obj draw it on the canvas applying first the drawing
;; scale and offset
;;
(define (vg:draw-xaxis drawing obj #!key (draw #t))
  (let* ((cnv (vg:drawing-cnv drawing))
	 (pts (vg:drawing-apply-scale drawing (vg:obj-pts obj)))
	 ;; (fill-color (vg:obj-fill-color obj))
	 (line-color (vg:obj-line-color obj))
	 (text       (vg:obj-text obj))
	 (font       (vg:obj-font obj))
	 (llx        (car pts))
	 (lly        (cadr pts))
	 (ulx        (caddr pts))
	 (uly        (cadddr pts))
	 (w          (- ulx llx))
	 (h          (- uly lly))
	 (text-xmax  #f)
	 (text-ymax  #f))
    (if draw 
	(let ((prev-background-color (canvas-background cnv))
	      (prev-foreground-color (canvas-foreground cnv)))
	;; (if fill-color
	;;     (begin
	;; 	(canvas-foreground-set! cnv fill-color)
	;; 	(canvas-box! cnv llx ulx lly uly))) ;; docs are all over the place on this one.;; w h)
	  (if line-color
	      (canvas-foreground-set! cnv line-color)
	      (if fill-color
		  (canvas-foreground-set! cnv prev-foreground-color)))
	  (canvas-line! cnv llx ulx lly uly)
	  (canvas-foreground-set! cnv prev-foreground-color)
	  (if text 
	      (let* ((prev-font    (canvas-font cnv))
		     (font-changed (and font (not (equal? font prev-font)))))
		(if font-changed (canvas-font-set! cnv font))
		(canvas-text! cnv (+ 2 llx)(+ 2 lly) text)
		(let-values (((xmax ymax)(canvas-text-size cnv text)))
		  (set! text-xmax xmax)(set! text-ymax ymax))
		(if font-changed (canvas-font-set! cnv prev-font))))))
    (print "text-xmax: " text-xmax " text-ymax: " text-ymax)
    (if (vg:obj-extents obj)
	(vg:obj-extents obj)
	(if (not text)
	    pts
	    (if (and text-xmax text-ymax)
		(let ((xt (list llx lly
				(max ulx (+ llx text-xmax))
				(max uly (+ lly text-ymax)))))
		  (vg:obj-extents-set! obj xt)
		  xt)
		(if cnv
		    (let-values (((xmax ymax)(canvas-text-size cnv text)))
		      (let ((xt (list llx lly
				      (max ulx (+ llx xmax))
				      (max uly (+ lly ymax)))))
			(vg:obj-extents-set! obj xt)
			xt))
		    pts)))))) ;; return extents 

;; given a rect obj draw it on the canvas applying first the drawing
;; scale and offset
;;
(define (vg:draw-text drawing obj #!key (draw #t))
  (let* ((cnv        (vg:drawing-cnv drawing))
	 (pts        (vg:drawing-apply-scale drawing (vg:obj-pts obj)))
	 (text       (vg:obj-text obj))
	 (font       (vg:obj-font obj))
	 (fill-color (vg:obj-fill-color obj))
	 (line-color (vg:obj-line-color obj))
	 (llx        (car pts)) 
	 (lly        (cadr pts)))
    (if draw 
	(let* ((prev-background-color (canvas-background cnv))
	       (prev-foreground-color (canvas-foreground cnv))
	       (prev-font             (canvas-font       cnv))
	       (font-changed    (and font (not (equal? font prev-font)))))
	  (if line-color
	      (canvas-foreground-set! cnv line-color)
	      (if fill-color
		  (canvas-foreground-set! cnv prev-foreground-color)))
	  (if font-changed (canvas-font-set! cnv font))
	  (canvas-text! cnv llx lly text)
	  ;; NOTE: we do not set the font back!!
	  (canvas-foreground-set! cnv prev-foreground-color)))
    (if cnv
	(if (eq? draw 'get-extents)
	    (let-values (((xmax ymax)(canvas-text-size cnv text)))
			(append pts (list (+ llx xmax)(+ lly ymax)))) ;; will be wrong if text is rotated?
	    (append pts pts))
	(append pts pts))))

(define (vg:draw drawing draw-mode . instnames)
  (let ((insts (vg:drawing-insts drawing))
	(res   '()))
    (for-each 
     (lambda (instname)
       (let* ((inst     (hash-table-ref/default insts instname #f)))
	 (if inst
	     (let* ((libname  (vg:inst-libname inst))
		    (compname (vg:inst-compname inst))
		    (comp     (vg:get-component drawing libname compname)))
	       ;; (print "comp: " comp)
	       (for-each
		(lambda (obj)
		  ;; (print "obj: " (vg:obj-pts obj))
		  (let ((obj-xfrmd (vg:map-obj drawing inst obj)))
		    ;; (print "obj-xfrmd: " (vg:obj-pts obj-xfrmd))
		    (set! res (cons (vg:draw-obj drawing obj-xfrmd draw: draw-mode) res)))) ;;
		(vg:comp-objs comp)))
	     (print "no such instance " instname))))
     (if (null? instnames)
	 (hash-table-keys insts)
	 instnames))
    res)) ;;  (hash-table-values insts))))
