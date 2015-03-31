(use ezxdisp srfi-18)

(define *ezx* (ezx-init 650 650 "Batch simulator"))
(require-library ezxgui)
(define *green*  (make-ezx-color 0 1 0)) 
(define *black*  (make-ezx-color 0 0 0))
(define *grey*   (make-ezx-color 0.1 0.1 0.1))
(define *blue*   (make-ezx-color 0 0 1)) 
(define *cyan*   (make-ezx-color 0 1 1))
(define *green*  (make-ezx-color 0 1 0))
(define *purple* (make-ezx-color 1 0 1))
(define *red*    (make-ezx-color 1 0 0))
(define *white*  (make-ezx-color 1 1 1))
(define *yellow* (make-ezx-color 1 1 0))

(define *user-colors-palette*
  (list 
   *green*
   *blue*
   *cyan*
   *purple*
   *red*
   *yellow*
   *black*))

(define *dark-green* (get-color "dark-green"))
(define *brown*      (get-color "brown"))

(ezx-select-layer *ezx* 1)
(ezx-wipe-layer *ezx* 1)

;; (ezx-str-2d *ezx* 30 30 "Hello" *white*)
;; (ezx-fillrect-2d *ezx* 100 100 120 120 *brown*)
(ezx-redraw *ezx*)

(define *last-draw* (current-milliseconds))
(define *draw-delta* 40) ;; milliseconds between drawing

(define (wait-for-next-draw-time)
  (let* ((cm    (current-milliseconds))
	 (delta (- *draw-delta* (- cm *last-draw*))))
    (if (> delta 0)
	(thread-sleep! (/ delta 1000)))
    (set! *last-draw* (current-milliseconds))))

(include "events.scm")

;; System spec (to be moved into loaded file)
;;
;;                           x  y  w gap  x-min x-max
(define *cpu-grid* (vector  500 50 15  2  500   600))
(define (make-cpu:grid)(make-vector 6))
(define *queues* (make-hash-table)) ;; name -> (list (list user duration num-cpus num-gigs) ... )
(define *cpus* (make-hash-table)) ;; cpu-name => (vector user job-len num-cpu mem x-loc y-loc)
(define *obj-locations* (make-hash-table)) ;; name -> (x y layer)
(define *queue-spec*
  (vector
   80  ;; start-x
   300 ;; start-y
   300 ;; delta-y how far to next queue
   15  ;; height
   400 ;; length
   ))
(define *use-log* #f)
(define *job-log-scale* 10)

;;======================================================================
;; Users
;;======================================================================

(define *user-colors* (make-hash-table))

(define (get-user-color user)
  (let ((color (hash-table-ref/default *user-colors* user #f)))
    (if color
	color
	(let* ((color-num (+ (length (hash-table-keys *user-colors*)) 1))
	       (color     (list-ref *user-colors-palette* color-num)))
	  (hash-table-set! *user-colors* user color)
	  color))))

;;======================================================================
;; Job Queues
;;======================================================================

;; jobs

(define (make-queue:job)(make-vector 4))
(define-inline (queue:job-get-user       vec)    (vector-ref  vec 0))
(define-inline (queue:job-get-duration   vec)    (vector-ref  vec 1))
(define-inline (queue:job-get-num-cpu    vec)    (vector-ref  vec 2))
(define-inline (queue:job-get-num-gigs   vec)    (vector-ref  vec 3))
(define-inline (queue:job-set-user!      vec val)(vector-set! vec 0 val))
(define-inline (queue:job-set-duration!  vec val)(vector-set! vec 1 val))
(define-inline (queue:job-set-num-cpu!   vec val)(vector-set! vec 2 val))
(define-inline (queue:job-set-num-gigs!  vec val)(vector-set! vec 3 val))

;; add a job to the queue
;;
(define (add-job queue-name user duration num-cpu num-gigs)
  (let* ((queue-dat (hash-table-ref/default *queues* queue-name '()))
	 (new-queue (append 
		     queue-dat
		     (list (vector user duration num-cpu num-gigs)))))
  (hash-table-set! *queues* queue-name new-queue)
  (draw-queue-jobs queue-name)))

;; peek for jobs to do in queue
;;
(define (peek-job queue-name)
  (let ((queue (hash-table-ref/default *queues* queue-name '())))
    (if (null? queue)
	#f
	(car queue))))

;; take job from queue
;;
(define (take-job queue-name)
  (let ((queue (hash-table-ref/default *queues* queue-name '())))
    (if (null? queue)
	#f
	(begin
	  (hash-table-set! *queues* queue-name (cdr queue))
	  (draw-queue-jobs queue-name)
	  (car queue)))))

;;======================================================================
;; CPUs
;;======================================================================

(define (make-cpu:dat)(make-vector 6 #f))
(define-inline (cpu:dat-get-user      vec)    (vector-ref  vec 0))
(define-inline (cpu:dat-get-job-len   vec)    (vector-ref  vec 1))
(define-inline (cpu:dat-get-num-cpu   vec)    (vector-ref  vec 2))
(define-inline (cpu:dat-get-mem       vec)    (vector-ref  vec 3))
(define-inline (cpu:dat-get-x         vec)    (vector-ref  vec 4))
(define-inline (cpu:dat-get-y         vec)    (vector-ref  vec 5))
(define-inline (cpu:dat-set-user!     vec val)(vector-set! vec 0 val))
(define-inline (cpu:dat-set-job-len!  vec val)(vector-set! vec 1 val))
(define-inline (cpu:dat-set-num-cpu!  vec val)(vector-set! vec 2 val))
(define-inline (cpu:dat-set-mem!      vec val)(vector-set! vec 3 val))
(define-inline (cpu:dat-set-x!        vec val)(vector-set! vec 4 val))
(define-inline (cpu:dat-set-y!        vec val)(vector-set! vec 5 val))

(define-inline (cpu:grid-get-x        vec)    (vector-ref  vec 0))
(define-inline (cpu:grid-get-y        vec)    (vector-ref  vec 1))
(define-inline (cpu:grid-get-w        vec)    (vector-ref  vec 2))
(define-inline (cpu:grid-get-gap      vec)    (vector-ref  vec 3))
(define-inline (cpu:grid-get-x-min    vec)    (vector-ref  vec 4))
(define-inline (cpu:grid-get-x-max    vec)    (vector-ref  vec 5))
(define-inline (cpu:grid-set-x!       vec val)(vector-set! vec 0 val))
(define-inline (cpu:grid-set-y!       vec val)(vector-set! vec 1 val))
(define-inline (cpu:grid-set-w!       vec val)(vector-set! vec 2 val))
(define-inline (cpu:grid-set-gap!     vec val)(vector-set! vec 3 val))
(define-inline (cpu:grid-set-x-min!   vec val)(vector-set! vec 4 val))
(define-inline (cpu:grid-set-x-max!   vec val)(vector-set! vec 5 val))

(define (add-cpu name num-cores mem)
  (let ((x     (cpu:grid-get-x     *cpu-grid*))
	(y     (cpu:grid-get-y     *cpu-grid*))
	(delta (+ (cpu:grid-get-w  *cpu-grid*)(cpu:grid-get-gap *cpu-grid*)))
	(x-max (cpu:grid-get-x-max *cpu-grid*)))
    (hash-table-set! *cpus* name (vector #f #f num-cores mem x y))
    (if (> x x-max)
	(begin
	  (cpu:grid-set-x! *cpu-grid* (cpu:grid-get-x-min *cpu-grid*))
	  (cpu:grid-set-y! *cpu-grid* (+ y delta)))
	(cpu:grid-set-x! *cpu-grid* (+ x delta)))))

;; draw grey box for each cpu on layer 2
;; jobs are drawn on layer 1
;;
(define (draw-cpus) ;; call once after init'ing all cpus
  (ezx-select-layer *ezx* 1)
  (ezx-wipe-layer   *ezx* 1)
  ;; draw time at upper right
  (ezx-str-2d *ezx* 20 20 (seconds->h:m:s *now*) *black*)
  (for-each
   (lambda (cpu)
     (let ((x (cpu:dat-get-x cpu))
	   (y (cpu:dat-get-y cpu))
	   (w (cpu:grid-get-w *cpu-grid*)))
       (ezx-rect-2d *ezx* x y (+ x w) (+ y w) *grey* 1)))
   (hash-table-values *cpus*))
  (ezx-redraw *ezx*))

(define (draw-jobs)
  ;; (draw-cpus)
  (ezx-select-layer *ezx* 2)
  (ezx-wipe-layer   *ezx* 2)
  (for-each
   (lambda (cpu)
     (let* ((x (cpu:dat-get-x cpu))
	    (y (cpu:dat-get-y cpu))
	    (w (cpu:grid-get-w *cpu-grid*))
	    (u (cpu:dat-get-user cpu)))
       (if u ;; job running if not #f
	   (let ((color (get-user-color u)))
	     (ezx-fillrect-2d *ezx* (+ x 2)(+ 2 y)(+ x 9) (+ y 9) color)))))
   (hash-table-values *cpus*))
  (ezx-redraw *ezx*))

(define (end-job cpu-name user)
  (let ((cpu (hash-table-ref/default *cpus* cpu-name #f)))
    (if cpu
	(let ((curr-user (cpu:dat-get-user cpu))) ;; if it is a user name then job is not done - error
	  (if (or (not curr-user)
		  (not (equal? curr-user user)))
	      (print "ERROR: cpu " cpu-name " not running job for " user "!")
	      (begin
		(cpu:dat-set-user!    cpu #f)
		(cpu:dat-set-job-len! cpu #f)
		(draw-jobs)))) ;; hash-table-set! *cpus* cpu-name (make-cpu:dat))))
	(print "ERROR: no cpu " cpu-name " found. Ensure it is registered before addressing it."))))

(define (run-job cpu-name job)
  (let* ((user    (queue:job-get-user     job))
	 (job-len (queue:job-get-duration job))
	 (cpu     (hash-table-ref/default *cpus* cpu-name #f)))
    (if cpu
	(let ((curr-user (cpu:dat-get-user cpu))) ;; if it is a user name then job is not done - error
	  (if curr-user
	      (begin
		(print "ERROR: cpu already busy! Adding more jobs not supported yet. " cpu-name)
		#f)
	      (begin
		(cpu:dat-set-user!    cpu user)
		(cpu:dat-set-job-len! cpu job-len)
		(draw-jobs)
		(hash-table-set! *cpus* cpu-name cpu)
		(event (+ *now* job-len) (lambda ()(end-job cpu-name user)))
		#t)))
	#f)))
    
(define (get-cpu)
  (let ((all-cpus (hash-table-keys *cpus*)))
    (if (null? all-cpus) 
	#f
	(let loop ((hed (car all-cpus))
		   (tal (cdr all-cpus)))
	  (if (cpu:dat-get-user (hash-table-ref/default *cpus* hed '(#f #f))) ;; if user is #f then cpu is available
	      (if (null? tal)
		  #f
		  (loop (car tal)(cdr tal)))
	      hed)))))
  
;;======================================================================
;; Animation
;;======================================================================

;; make-vector-record  queue spec x y delta-y height length
(define (make-queue:spec)(make-vector 5))
(define-inline (queue:spec-get-x         vec)    (vector-ref  vec 0))
(define-inline (queue:spec-get-y         vec)    (vector-ref  vec 1))
(define-inline (queue:spec-get-delta-y   vec)    (vector-ref  vec 2))
(define-inline (queue:spec-get-height    vec)    (vector-ref  vec 3))
(define-inline (queue:spec-get-length    vec)    (vector-ref  vec 4))
(define-inline (queue:spec-set-x!        vec val)(vector-set! vec 0 val))
(define-inline (queue:spec-set-y!        vec val)(vector-set! vec 1 val))
(define-inline (queue:spec-set-delta-y!  vec val)(vector-set! vec 2 val))
(define-inline (queue:spec-set-height!   vec val)(vector-set! vec 3 val))
(define-inline (queue:spec-set-length!   vec val)(vector-set! vec 4 val))

;; queues are drawn on layer 3 but boxes (jobs) are drawn on the numbered layer
;;
(define (draw-queues)
  (let* ((text-offset 3)
	 (queue-names (sort (hash-table-keys *queues*) string>=?))
	 (start-x (vector-ref *queue-spec* 0))
	 (text-x  (+ start-x text-offset))
	 (delta-y (vector-ref *queue-spec* 1))
	 (delta-x (vector-ref *queue-spec* 2))
	 (height  (vector-ref *queue-spec* 3))
	 (length  (vector-ref *queue-spec* 4))
	 (end-x   (+ start-x length)))
    (ezx-select-layer *ezx* 3)
    (ezx-wipe-layer   *ezx* 3)
    (let loop ((y       (vector-ref *queue-spec* 1))
	       (qname   (car queue-names))
	       (tail    (cdr queue-names))
	       (layer   4))
      (print "Drawing queue at x=" start-x ", y=" y)
      (ezx-fillrect-2d  *ezx* start-x y end-x (+ y height) *brown*)
      (ezx-str-2d       *ezx* text-x (- (+ y height) text-offset) qname *white*)
      (hash-table-set! *obj-locations* qname (list start-x y layer))
      (if (not (null? tail))
	  (loop (+ y height delta-y)
		(car tail)
		(cdr tail)
		(+  layer 1))))
    (ezx-redraw *ezx*)))

;; compress queue data to (vector user count) list
;;
(define (draw-queue-compress-queue-data queue-dat)
  (let loop ((hed  (car queue-dat))
	     (tal  (cdr queue-dat))
	     (curr #f) ;; (vector name count)
	     (res  '()))
    (let ((user (queue:job-get-user hed)))
      (cond
       ((not curr) ;; first time through only?
	(if (null? tal)
	    (append res (list (vector user 1)))
	    (loop (car tal)(cdr tal)(vector user 1) res)))
       ((equal? (vector-ref curr 0) user) 
	(vector-set! curr 1 (+ (vector-ref curr 1) 1))
	(if (null? tal)
	    (append res (list curr))
	    (loop (car tal)(cdr tal) curr res)))
       (else ;; names are different, add curr to queue and create new curr
	(let ((newcurr (vector user 1)))
	  (if (null? tal)
	      (append res (list newcurr))
	      (loop (car tal)(cdr tal) newcurr (append res (list curr))))))))))

;; draw jobs for a given queue
;;
(define (draw-queue-jobs queue-name)
  (let* ((queue-dat (hash-table-ref/default *queues*        queue-name #f))  ;; list of jobs in the queue
	 (obj-spec  (hash-table-ref/default *obj-locations* queue-name #f)))  ;; x, y etc. of the drawn queue
    (if obj-spec
	(let ((origin-x  (list-ref obj-spec 0))
	      (origin-y  (list-ref obj-spec 1))
	      (bar-width 10)
	      (queue-len (queue:spec-get-length *queue-spec*))
	      (layer     (list-ref obj-spec 2)))
	  (ezx-select-layer *ezx* layer)
	  (ezx-wipe-layer   *ezx* layer)
	  (if (not (null? queue-dat))
	      (let ((res (draw-queue-compress-queue-data queue-dat)))
		(if (not (null? res))
		    (let loop ((hed (car res))
			       (tal (cdr res))
			       (x2   (+ origin-x queue-len)))
		      (let* ((user (vector-ref hed 0))
			     (h    (let ((numjobs (vector-ref hed 1)))
				     (if *use-log*
					 (inexact->exact (round (log (+ 1 (* *job-log-scale* numjobs)))))
					 numjobs)))
			     (x1   (- x2  bar-width))
			     (y2   (- origin-y h)))
			;; (print "x1 " x1 ", origin-y " origin-y ", x2 " x2 ", y2 " y2)
			(ezx-fillrect-2d *ezx* x1 y2 x2 origin-y (get-user-color user))
			(if (not (null? tal))
			    (loop (car tal)(cdr tal) x1)))))
		(ezx-redraw *ezx*)))))))
	  
(let* ((args  (argv))
       (fname (if (> (length args) 1)
		  (cadr args)
 		  "default.scm")))
  (load (if (file-exists? fname) fname "default.scm")))
