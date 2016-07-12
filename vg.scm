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
(defstruct vg:inst    libname compname theta xoff yoff scale mirrx mirry call-back)
(defstruct vg:drawing libs insts cnv) ;; libs: hash of name->lib, insts: hash of instname->inst

;; inits
;;
(define (vg:comp-new)
  (make-vg:comp objs: '() name: #f file: #f))

(define (vg:lib-new)
  (make-vg:lib comps: (make-hash-table)))

(define (vg:drawing-new)
  (make-vg:drawing libs: (make-hash-table) insts: (make-hash-table)))

;; make a rectangle obj
;;
(define (vg:make-rect x1 y1 x2 y2 #!key (line-color #f)(fill-color #f))
  (make-vg:obj type: 'r pts: (list x1 y1 x2 y2) text: #f line-color: line-color fill-color: fill-color))

;; add obj to comp
;;
(define (vg:add-objs-to-comp comp . objs)
  (vg:comp-objs-set! comp (append (vg:comp-objs comp) objs)))

;; add comp to lib
;;
(define (vg:add-comp-to-lib lib compname comp)
  (hash-table-set! (vg:lib-comps lib) compname comp))

;; instanciate component in drawing
;;
(define (vg:instantiate drawing libname compname instname xoff yoff t #!key (scale 1)(mirrx #f)(mirry #f))
  (let ((inst (make-vg:inst libname: libname compname: compname xoff: xoff yoff: yoff theta: t scale: scale mirrx: mirrx mirry: mirry)) )
    (hash-table-set! (vg:drawing-insts drawing) instname inst)))

;; get component from drawing (look in apropriate lib) given libname and compname
(define (vg:get-component drawing libname compname)
  (let* ((lib  (hash-table-ref (vg:drawing-libs drawing) libname))
	 (inst (hash-table-ref (vg:lib-comps lib) compname)))
    inst))

;; register lib with drawing
;;
(define (vg:add-lib drawing libname lib)
  (hash-table-set! (vg:drawing-libs drawing) libname lib))

;;======================================================================
;; map objects given offset, scale and mirror
;;======================================================================

(define (vg:map-obj xoff yoff theta scale mirrx mirry obj)
  (case (vg:obj-type obj)
    ((r)(vg:map-rect xoff yoff theta scale mirrx mirry obj))
    (else #f)))

(define (vg:map-rect xoff yoff theta scale mirrx mirry obj)
  (let ((res (make-vg:obj type:       'r
			  fill-color: (vg:obj-fill-color obj)
			  text:       (vg:obj-text       obj)
			  line-color: (vg:obj-line-color obj)
			  font:       (vg:obj-font       obj)))
	(pts (vg:obj-pts obj)))
    (vg:obj-pts-set! res 
		     (list (+ xoff (car pts))
			   (+ yoff (cadr pts))
			   (+ xoff (caddr pts))
			   (+ yoff (cadddr pts))))
    res))

;;======================================================================
;; Unravel and draw the objects
;;======================================================================

(define (vg:draw-obj cnv obj)
  (print "obj type: " (vg:obj-type obj))
  (case (vg:obj-type obj)
    ((r)(vg:draw-rect cnv obj))))

(define (vg:draw-rect cnv obj)
  (let* ((pts (vg:obj-pts obj))
	 (llx (car pts))
	 (lly (cadr pts))
	 (urx (caddr pts))
	 (ury (cadddr pts)))
    (print "(canvas-rectangle! " cnv " " llx " " urx " " lly " " ury ")")
    (canvas-rectangle! cnv llx urx lly ury)
    ))

(define (vg:draw drawing)
  (let ((insts (vg:drawing-insts drawing))
	(cnv   (vg:drawing-cnv   drawing)))
    (for-each 
     (lambda (inst)
       (let* ((xoff     (vg:inst-xoff inst))
	      (yoff     (vg:inst-yoff inst))
	      (theta    (vg:inst-theta inst))
	      (scale    (vg:inst-scale inst))
	      (mirrx    (vg:inst-mirrx inst))
	      (mirry    (vg:inst-mirry inst))
	      (libname  (vg:inst-libname inst))
	      (compname (vg:inst-compname inst))
	      (comp     (vg:get-component drawing libname compname)))
	 (print "comp: " comp)
	 (for-each
	  (lambda (obj)
	    (print "obj: " obj)
	    (vg:draw-obj cnv (vg:map-obj xoff yoff theta scale mirrx mirry obj)))
	  (vg:comp-objs comp))))
    (hash-table-values insts))))
