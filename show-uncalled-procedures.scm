;; this code will read scm files from $PWD and return a list of defined procedures that are not called by any other procedures
;;   -- be advised:
;;      * this may be fooled by macros, since this code does not take them into account.
;;      * this code does only checks for form (define (<procname> ... ) <body>)
;;           so it excludes from reckoning
;;               - generated functions, as in things like foo-set! from defstructs,
;;               - define-inline, (
;;               - define procname (lambda ..
;;               - etc...

;; gotta compile with csc, doesn't work with csi -s for whatever reason

(use srfi-69)
(use matchable)
(use utils)
(use ports)
(use extras)
(use srfi-1)
(use posix)
(use srfi-12)

;; turn scheme file to a list of sexps, sexps of interest will be in the form of (define (<procname> <args>) <body> )
(define (load-scm-file scm-file)
  ;;(print "load "scm-file)
  (handle-exceptions
   exn
   '()
   (with-input-from-string
       (conc "("
             (with-input-from-file scm-file read-all)
             ")" )
     read)))

;; extract a list of procname, filename, args and body of procedures defined in filename, input from load-scm-file
(define (get-toplevel-procs+file+args+body filename)
  (let* ((scm-tree (load-scm-file filename))
         (procs
          (filter identity
                  (map
                   (match-lambda 
                    [('define ('uses args ...) body ...) #f] ;; filter out (define (uses ...
                    [('define ('unit args ...) body ...) #f] ;; filter out (define (unit ...
                    [('define ('prefix args ...) body ...) #f] ;; filter out (define (prefix ...
                    [('define (defname args ...) body ...) ;; match (define (procname <args>) <body>)
                     (if (atom? defname) ;; filter out things we dont understand (procname is a list, what??)
                         (list defname filename args body)
                         #f)]
                    [else #f] ) scm-tree))))
    procs))


;; given a sexp, return a flat lost of atoms in that sexp
(define (get-atoms-in-body body)
  (cond
   ((null? body) '())
   ((atom? body) (list body))
   (else
    (apply append (map get-atoms-in-body body)))))

;;  given a file, return a list of procname, file, list of atoms in said procname
(define (get-procs+file+atoms file)
  (let* ((toplevel-proc-items (get-toplevel-procs+file+args+body file))
         (res
          (map
           (lambda (item)
             (let* ((proc (car item))
                    (file (cadr item))
                    (args (caddr item))
                    (body (cadddr item))
                    (atoms (append (get-atoms-in-body args) (get-atoms-in-body body))))
               (list proc file atoms)))
           toplevel-proc-items)))
    res))

;; uniquify a list of atoms 
(define (unique-atoms lst)
  (let loop ((lst (flatten lst)) (res '()))
    (if (null? lst)
        (reverse res)
        (let ((c (car lst)))
          (loop (cdr lst) (if (member c res) res (cons c res)))))))

;; given a list of procname, filename, list of procs called from procname, cross reference and reverse
;; returning alist mapping procname to procname that calls said procname
(define (get-callers-alist all-procs+file+calls)
  (let* ((all-procs (map car all-procs+file+calls))
         (caller-ht (make-hash-table))) 
    ;; let's cross reference with a hash table
    (for-each (lambda (proc) (hash-table-set! caller-ht proc '())) all-procs)
    (for-each (lambda (item)
               (let* ((proc (car item))
                      (file (cadr item))
                      (calls (caddr item)))
                 (for-each (lambda (callee)
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

;; read all scm files in cwd, pretty print a list of procs that are not called
(define (get-xref all-scm-files)
  (let* ((all-procs+file+atoms
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
         (callers (get-callers-alist all-procs+file+calls))) ;; this is a handy cross-reference of callees to callers.  could be used elsewhere
    callers))

(define (main)
  (let* ((all-scm-files (glob "*.scm"))
         (xref (get-xref all-scm-files))
         (dangling-procs
          (map car (filter (lambda (x) (equal? 1 (length x))) xref))))
    (for-each print dangling-procs) ;; our product.
    ))

(main)
