;;======================================================================
;; Copyright 2017, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================
(define (readlines filename)
  (call-with-input-file filename
    (lambda (p)
      (let loop ((line (read-line p))
                 (result '()))
        (if (eof-object? line)
            (reverse result)
            (loop (read-line p) (cons line result)))))))

(define (pages:log session db shared)
  (let* ((dbh         (s:db))
	 (id      (s:get-param 'testid))
         (tests (pgdb:get-test-by-id dbh id)))
     
    (if (eq? (length tests) 1)
    (begin
    (s:div 'class "col_12"
	   (s:fieldset
	   (conc "Show a runs for Target: " )
             (let* ((test (car tests))
		   (html-path (conc (vector-ref test 2) "/" (vector-ref test 3)))
                    (html-data (readlines html-path)))
                  (s:p html-data)))))
      (begin 
         (s:div 'class "col_12"
    "Log not found")))
))
		      
