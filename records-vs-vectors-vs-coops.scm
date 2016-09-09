;; (include "vg.scm")

;; (declare (uses vg))

(use foof-loop defstruct coops)

(defstruct obj     type fill-color angle)

(define (make-vg:obj)(make-vector 3))
(define-inline (vg:obj-get-type         vec)    (vector-ref  vec 0))
(define-inline (vg:obj-get-fill-color   vec)    (vector-ref  vec 1))
(define-inline (vg:obj-get-angle        vec)    (vector-ref  vec 2))
(define-inline (vg:obj-set-type!        vec val)(vector-set! vec 0 val))
(define-inline (vg:obj-set-fill-color!  vec val)(vector-set! vec 1 val))
(define-inline (vg:obj-set-angle!       vec val)(vector-set! vec 2 val))

(use simple-exceptions)
(define vgs:obj-exn (make-exception "wrong record type, expected vgs:obj." 'assert))
(define (make-vgs:obj)(let ((v (make-vector 4)))(vector-set! v 0 'vgs:obj) v))
(define-inline (vgs:obj-type             vec)(if (eq? (vector-ref vec 0) 'vgs:obj)(vector-ref  vec 1)(raise (vgs:obj-exn 'vgs:obj-type 'xpr))))
(define-inline (vgs:obj-fill-color       vec)(if (eq? (vector-ref vec 0) 'vgs:obj)(vector-ref  vec 2)(raise (vgs:obj-exn 'vgs:obj-fill-color 'xpr))))
(define-inline (vgs:obj-angle            vec)(if (eq? (vector-ref vec 0) 'vgs:obj)(vector-ref  vec 3)(raise (vgs:obj-exn 'vgs:obj-angle 'xpr))))
(define-inline (vgs:obj-type-set!        vec val)(if (eq? (vector-ref vec 0) 'vgs:obj)(vector-set! vec 1 val)(raise (vgs:obj-exn 'type))))
(define-inline (vgs:obj-fill-color-set!  vec val)(if (eq? (vector-ref vec 0) 'vgs:obj)(vector-set! vec 2 val)(raise (vgs:obj-exn 'fill-color))))
(define-inline (vgs:obj-angle-set!       vec val)(if (eq? (vector-ref vec 0) 'vgs:obj)(vector-set! vec 3 val)(raise (vgs:obj-exn 'angle))))

(define-class <vgc> ()
  ((type)
   (fill-color)
   (angle)))


;; first use raw vectors
(print "Using vectors")
(time
 (loop ((for r (up-from 0 (to 255))))
       (loop ((for g (up-from 0 (to 255))))
	     (loop ((for b (up-from 0 (to 255))))
		   (let ((obj (make-vg:obj)))
		     (vg:obj-set-type! obj 'abc)
		     (vg:obj-set-fill-color! obj "green")
		     (vg:obj-set-angle! obj 135)
		     (let ((a (vg:obj-get-type obj))
			   (b (vg:obj-get-fill-color obj))
			   (c (vg:obj-get-angle obj)))
		       obj))))))

;; first use raw vectors with safe mode
(print "Using vectors (safe mode)")
(time
 (loop ((for r (up-from 0 (to 255))))
       (loop ((for g (up-from 0 (to 255))))
	     (loop ((for b (up-from 0 (to 255))))
		   (let ((obj (make-vgs:obj)))
		     ;; (badobj (make-vector 20)))
		     (vgs:obj-type-set! obj 'abc)
		     (vgs:obj-fill-color-set! obj "green")
		     (vgs:obj-angle-set! obj 135)
		     (let ((a (vgs:obj-type obj))
			   (b (vgs:obj-fill-color obj))
			   (c (vgs:obj-angle obj)))
		       obj))))))

;; first use defstruct
(print "Using defstruct")
(time
 (loop ((for r (up-from 0 (to 255))))
       (loop ((for g (up-from 0 (to 255))))
	     (loop ((for b (up-from 0 (to 255))))
		   (let ((obj (make-obj)))
		     (obj-type-set! obj 'abc)
		     (obj-fill-color-set! obj "green")
		     (obj-angle-set! obj 135)
		     (let ((a (obj-type obj))
			   (b (obj-fill-color obj))
			   (c (obj-angle obj)))
		       obj))))))
		   

;; first use defstruct
(print "Using coops")
(time
 (loop ((for r (up-from 0 (to 255))))
       (loop ((for g (up-from 0 (to 255))))
	     (loop ((for b (up-from 0 (to 255))))
		   (let ((obj (make <vgc>)))
		     (set! (slot-value obj 'type) 'abc)
		     (set! (slot-value obj 'fill-color) "green")
		     (set! (slot-value obj 'angle) 135)
		     (let ((a (slot-value obj 'type))
			   (b (slot-value obj 'fill-color))
			   (c (slot-value obj 'angle)))
		       obj))))))
