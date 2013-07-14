
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
(use regex)
(use srfi-69)

;; Read a non-compressed gnumeric file
(define (txtdb:read-gnumeric-xml fname)
  (with-input-from-file fname
    (lambda ()
      (ssax:xml->sxml (current-input-port) '()))))

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

(define (remove-section dat section)
  (if (null? dat)
      '()
      (let loop ((hed (car dat))
		 (tal (cdr dat))
		 (res '()))
	(let ((newres (if (and (list? hed)
			       (not (null? hed))
			       (equal? (car hed) section))
			  res
			  (cons hed res))))
	  (if (null? tal)
	      (reverse newres)
	      (loop (car tal)(cdr tal) newres))))))

(define (list-sections dat)
  (filter (lambda (x)(and x))
	  (map (lambda (section)
		 (if (and (list? section)
			  (not (null? section)))
		     (car section)
		     #f))
	       dat)))

(define (string->safe-filename str)
  (string-substitute (regexp " ") "_" str #t))

(define (sheet->txtdb dat targdir)
  (let ((sheet-name  (string->safe-filename (car (find-section dat 'http://www.gnumeric.org/v10.dtd:Name))))
	(cells       (find-section dat 'http://www.gnumeric.org/v10.dtd:Cells))
	(remaining   (remove-section (remove-section dat 'http://www.gnumeric.org/v10.dtd:Name)
				     'http://www.gnumeric.org/v10.dtd:Cells))
	(rownums     (make-hash-table))  ;; num -> name
	(colnums     (make-hash-table))  ;; num -> name
	(cols        (make-hash-table))) ;; name -> ( (name val) ... )
    (for-each (lambda (cell)
		(let ((rownum  (string->number (car (find-section cell 'Row))))
		      (colnum  (string->number (car (find-section cell 'Col))))
		      (valtype (let ((res (find-section cell 'ValueType)))
				 (if res (car res) #f)))
		      (value   (let ((res (cdr (filter (lambda (x)(not (list? x))) cell))))
				 (if (null? res) "" (car res)))))
		  ;; If colnum is 0 Then this is a row name, if rownum is 0 then this is a col name
		  (cond
		   ((eq? 0 colnum) ;; a blank in column zero is handled with the special name "row-N"
		    (hash-table-set! rownums rownum (if (equal? value "")
							(conc "row-" rownum)
							value)))
		   ((eq? 0 rownum)
		    (hash-table-set! colnums colnum (if (equal? value "")
							(conc "col-" colnum)
							value)))
		   (else
		    (let ((colname (hash-table-ref/default colnums colnum (conc "col-" colnum)))
			  (rowname (hash-table-ref/default rownums rownum (conc "row-" rownum))))
		      (hash-table-set! cols colname (cons (list rowname value) 
							  (hash-table-ref/default cols colname '()))))))))
	      cells)
    (let ((ref-colnums (map (lambda (c)
			      (list (cdr c)(car c)))
			    (hash-table->alist colnums))))
      (with-output-to-file (conc targdir "/" sheet-name ".dat")
	(lambda ()
	  (for-each (lambda (colname)
		      (print "[" colname "]")
		      (for-each (lambda (row)
				  (print (car row) " " (cadr row)))
				(reverse (hash-table-ref cols colname)))
		      (print))
		    (sort (hash-table-keys cols)(lambda (a b)
						  (let ((colnum-a (assoc a ref-colnums))
							(colnum-b (assoc b ref-colnums)))
						    (if (and colnum-a colnum-b)
							(< (cadr colnum-a)(cadr colnum-b))
							(if (and (string? a)
								 (string? b))
							    (string< a b))))))))))
    (with-output-to-file (conc targdir "/sxml/" sheet-name ".sxml")
      (lambda ()
	(pp remaining)))))


(define (extract-txtdb dat targdir)
  (let* ((wrkbk   (find-section   dat   'http://www.gnumeric.org/v10.dtd:Workbook))
	 (wrk-rem (remove-section dat   'http://www.gnumeric.org/v10.dtd:Workbook))
	 (sheets  (find-section   wrkbk 'http://www.gnumeric.org/v10.dtd:Sheets))
	 (sht-rem (remove-section wrkbk 'http://www.gnumeric.org/v10.dtd:Sheets)))
    (create-directory (conc targdir "/sxml") #t)
    (with-output-to-file (conc targdir "/sxml/workbook.sxml")
      (lambda ()
	(pp wrk-rem)))
    (with-output-to-file (conc targdir "/sxml/sheets.sxml")
      (lambda ()
	(pp sht-rem)))
    (for-each (lambda (sheet)
		(sheet->txtdb sheet targdir))
	      sheets)))

#|  
 (define x (txtdb:read-gnumeric-xml "testdata-stripped.xml"))



;; Write out sxml
(with-output-to-file "testdata.sxml" (lambda()(pp x)))


;; (serialize-sxml a output: "new.xml")
(with-output-to-file "testdata-stripped.xml" (lambda ()(print (sxml-serializer#serialize-sxml y))))

;; Read in sxml file
(with-input-from-file "testdata.sxml" (lambda ()(set! y (read))))

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
|#