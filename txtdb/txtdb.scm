
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(use ssax)
(use sxml-serializer)
(use sxml-modifications)

;; Read a non-compressed gnumeric file
(define (txtdb:read-gnumeric-xml fname)
  (with-input-from-file fname
    (lambda ()
      (ssax:xml->sxml (current-input-port) '()))))

 (define x (txtdb:read-gnumeric-xml "testdata-stripped.xml"))



;; Write out sxml
(with-output-to-file "testdata.sxml" (lambda()(pp x)))


;; (serialize-sxml a output: "new.xml")
(with-output-to-file "testdata-stripped.xml" (lambda ()(print (sxml-serializer#serialize-sxml y))))

;; Read in sxml file
(with-input-from-file "testdata.sxml" (lambda ()(set! y (read))))

(define (find-section dat section #!key (depth 0))
  (let loop ((hed   (car dat))
	     (tal   (cdr dat)))   
    (if (list? hed)
	(let ((res (find-section hed section depth: (+ depth 1))))
	  (if res 
	      res
	      (if (null? tal)
		  #f
		  (loop (car tal)(cdr tal)))))
	(if (eq? hed section)
	    tal
	    (if (null? tal)
		#f
		(loop (car tal)(cdr tal)))))))
  
(find-section x 'http://www.gnumeric.org/v10.dtd:Workbook)
