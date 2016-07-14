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

(use defstruct)

(declare (unit vg))
(use canvas-draw iup)
(import canvas-draw-iup)
;; structs
;;
(defstruct vg:lib     comps)
(defstruct vg:comp    objs name file)
(defstruct vg:obj     type pts fill-color text line-color call-back font)
(defstruct vg:inst    libname compname theta xoff yoff scalex scaley mirrx mirry call-back cache)
(defstruct vg:drawing libs insts scalex scaley xoff yoff cnv cache) ;; libs: hash of name->lib, insts: hash of instname->inst

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

;;   (vg:inst-apply-scale 
;;    inst
;;    (vg:drawing-apply-scale drawing lstxy)))

;; make a rectangle obj
;;
(define (vg:make-rect x1 y1 x2 y2 #!key (line-color #f)(fill-color #f))
  (make-vg:obj type: 'r pts: (list x1 y1 x2 y2) text: #f line-color: line-color fill-color: fill-color))

;; get extents, use knowledge of type ...
;;
(define (vg:obj-get-extents obj)
  (let ((type (vg:obj-type obj)))
    (case type
      ((r)(vg:rect-get-extents obj)))))

(define (vg:rect-get-extents obj)
  (vg:obj-pts obj)) ;; extents are just the points for a rectangle

;;======================================================================
;; components
;;======================================================================

;; add obj to comp
;;
(define (vg:add-objs-to-comp comp . objs)
  (vg:comp-objs-set! comp (append (vg:comp-objs comp) objs)))

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

;; get component from drawing (look in apropriate lib) given libname and compname
(define (vg:get-component drawing libname compname)
  (let* ((lib  (hash-table-ref (vg:drawing-libs drawing) libname))
	 (inst (hash-table-ref (vg:lib-comps lib) compname)))
    inst))

(define (vg:component-get-extents comp)
  (let ((llx #f)
	(lly #f)
	(ulx #f)
	(uly #f)
	(objs (vg:comp-objs comp)))
    (for-each
     (lambda (obj)
       (let* ((extents (vg:get-extents obj))
	      (ollx    (list-ref extents 0))
	      (olly    (list-ref extents 1))
	      (oulx    (list-ref extents 2))
	      (ouly    (list-ref extents 3)))
	 (if (or (not llx)(< ollx llx))(set! llx ollx))
	 (if (or (not lly)(< olly llx))(set! llx ollx))
	 (if (or (not ulx)(< ollx llx))(set! llx ollx))
	 (if (or (not uly)(< ollx llx))(set! llx ollx))))
     objs)
    (list llx lly ulx uly)))


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
    ((r)(vg:map-rect drawing inst obj))
    (else #f)))

;; given a drawing and a inst map a rectangle to it screen coordinates
;;
(define (vg:map-rect drawing inst obj)
  (let ((res (make-vg:obj type:       'r
			  fill-color: (vg:obj-fill-color obj)
			  text:       (vg:obj-text       obj)
			  line-color: (vg:obj-line-color obj)
			  font:       (vg:obj-font       obj)))
	(pts (vg:obj-pts obj)))
    (vg:obj-pts-set! res (vg:drawing-inst-apply-scale-offset drawing inst pts))
    (vg:drawing-cache-set! drawing (cons res (vg:drawing-cache drawing) ))
    res))

;;======================================================================
;; Unravel and draw the objects
;;======================================================================

(define (vg:draw-obj drawing obj)
  (print "obj type: " (vg:obj-type obj))
  (case (vg:obj-type obj)
    ((r)(vg:draw-rect drawing obj))))

;; given a rect obj draw it on the canvas applying first the drawing
;; scale and offset
;;
(define (vg:draw-rect drawing obj)
  (let* ((cnv (vg:drawing-cnv drawing))
	 (pts (vg:drawing-apply-scale drawing (vg:obj-pts obj)))
	 (llx (car pts))
	 (lly (cadr pts))
	 (ulx (caddr pts))
	 (uly (cadddr pts)))
    (print "pts: " pts)
    (canvas-rectangle! cnv llx ulx lly uly)))

(define (vg:draw drawing)
  (let ((insts (vg:drawing-insts drawing)))
    (for-each 
     (lambda (inst)
       (let* ((libname  (vg:inst-libname inst))
	      (compname (vg:inst-compname inst))
	      (comp     (vg:get-component drawing libname compname)))
	 (print "comp: " comp)
	 (for-each
	  (lambda (obj)
	    (print "obj: " (vg:obj-pts obj))
	    (let ((obj-xfrmd (vg:map-obj drawing inst obj)))
	      (print "obj-xfrmd: " (vg:obj-pts obj-xfrmd))
	      (vg:draw-obj drawing obj-xfrmd))) ;;
	  (vg:comp-objs comp))))
    (hash-table-values insts))))
