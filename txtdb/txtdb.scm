
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
(define sheets (find-section x 'http://www.gnumeric.org/v10.dtd:Sheets))

(define sheet1 (car sheets))
(define cells-sheet1 (find-section sheet1 'http://www.gnumeric.org/v10.dtd:Cells))
(map (lambda (c)(find-section c 'Row)) cells-sheet1)

(for-each (lambda (cell)
	    (let* ((len (length cell))
		   (row (car (find-section cell 'Row)))
		   (col (car (find-section cell 'Col)))
		   (val (let ((res (cdr (filter (lambda (x)(not (list? x))) cell))))
			  (if (null? res) "" (car res)))))
	      (print "Row=" row " col=" col " val=" val)))
	  cells-sheet1)


(map (lambda (c)(filter (lambda (x)(not (list? x))) c)) cells-sheet1)
