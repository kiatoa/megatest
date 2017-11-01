(include "codescanlib.scm")

(define (show-danglers)
  (let* ((all-scm-files (glob "*.scm"))
         (xref (get-xref all-scm-files))
         (dangling-procs
          (map car (filter (lambda (x) (equal? 1 (length x))) xref))))
    (for-each print dangling-procs) ;; our product.
    ))

(show-danglers)

    
