#!/p/foundry/env/pkgs/megatest/1.64/chicken-4.10.0/bin/csi -s

(use srfi-69)
(use matchable)
(use utils)
(use ports)
(use extras)
(use srfi-1)
(use posix)
(use srfi-12)
(define (load-scm-file scm-file)
  ;;(print "load "scm-file)
  (handle-exceptions
   exn
   (begin
     ;;(print "  - problem with "scm-file"; skip it.")
     '())
   (with-input-from-string (conc "(" (with-input-from-file scm-file read-all) ")" ) read)))

(define (get-toplevel-procs+file+args+body filename)

  (let* ((scm-tree (load-scm-file filename))
         (procs
          (filter identity
                  (map (lambda (x)
                         (match x
                           [(define ('uses args ...) body ...) #f]
                           [(define ('unit args ...) body ...) #f]
                           [(define ('prefix args ...) body ...) #f]
                           [(define (defname args ...) body ...)
                            (if (atom? defname)
                                (list defname filename args body)
                                #f)]
                           [else #f] )) scm-tree)))
         )
    procs))

(define (get-atoms-in-body body)
  (cond
   ((null? body) '())
   ((atom? body) (list body))
   (else
    (apply append (map get-atoms-in-body body)))))

(define (get-procs+file+atoms file)
  (map
   (lambda (item)
     (let* ((proc (car item))
            (file (cadr item))
            (args (caddr item))
            (body (cadddr item))
            (atoms (append (get-atoms-in-body args) (get-atoms-in-body body))))
       (list proc file atoms)))
   (get-toplevel-procs+file+args+body file)))

(define (unique-atoms lst)
  (let loop ((lst (flatten lst)) (res '()))
    (if (null? lst)
        (reverse res)
        (let ((c (car lst)))
          (loop (cdr lst) (if (member c res) res (cons c res)))))))

(define (get-callers-alist all-procs+file+calls)
  (let* ((all-procs (map car all-procs+file+calls))
         (caller-ht (make-hash-table)))
    (for-each (lambda (proc) (hash-table-set! caller-ht proc '())) all-procs)
    (for-each (lambda (item)
               (let* ((proc (car item))
                      (file (cadr item))
                      (calls (caddr item)))
                 (for-each (lambda (callee)
                             ;(print "callee: "callee)
                             ;(exit 1)
                             (hash-table-set! caller-ht callee
                                              (cons proc
                                                    (hash-table-ref caller-ht callee))))
                           calls)))
              all-procs+file+calls)
    (map (lambda (x)
           (let ((k (car x))
                 (r (unique-atoms (cdr x))))
             (cons k r)))                    
         (hash-table->alist caller-ht))))


(let* ((all-scm-files (glob "*.scm"))
       (all-procs+file+atoms
        (apply append (map get-procs+file+atoms all-scm-files)))
       ;(foo (begin
       ;       (pp all-procs+file+atoms)
       ;       (exit 1)))
       (all-procs (map car all-procs+file+atoms))
       ;(bar (begin (pp all-procs) (exit 1)))
       (all-procs+file+calls  ; proc calls things in calls list
        (map (lambda (item)
               (let* ((proc (car item))
                      (file (cadr item))
                      (atoms (caddr item))
                      (calls
                       (filter identity
                               (map
                                (lambda (x)
                                  (if (and ;; (not (equal? x proc))  ;; uncomment to prevent listing self
                                           (member x all-procs))
                                      x
                                      #f))
                                atoms))))
                 (list proc file calls)))
             all-procs+file+atoms))
       (callers (get-callers-alist all-procs+file+calls))
       (singletons (filter (lambda (x) (equal? 1 (length x))) callers))
       )
  
  (pp singletons))
