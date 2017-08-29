(include "codescanlib.scm")

;; show call paths for named procedure
(define (traceback-proc in-procname)
  (letrec* ((all-scm-files (glob "*.scm"))
            (xref (get-xref all-scm-files))
            (have (alist-ref (string->symbol in-procname) xref eq? #f))
            (lookup (lambda (path procname depth)
                      (let* ((upcone-temp (filter (lambda (x)
                                                    (eq? procname (car x)))
                                                  xref))
                             (upcone-temp2 (cond
                                            ((null? upcone-temp) '())
                                            (else (cdar upcone-temp))))
                             (upcone (filter
                                      (lambda (x) (not (eq? x procname)))
                                      upcone-temp2))
                             (uppath (cons procname path))
                             (updepth (add1 depth)))
                        (if (null? upcone)
                            (print  uppath)
                            (for-each (lambda (x)
                                        (if (not (member procname path))
                                            (lookup uppath x updepth) ))
                                      upcone))))))
           (if have
               (lookup '() (string->symbol in-procname) 0)
               (print "no such func - "in-procname))))


(if (eq? 1 (length (command-line-arguments)))
    (traceback-proc (car (command-line-arguments)))
    (print "Usage: trackback <procedure name>"))

(exit 0)
    
