
;; Copyright 2006-2013, Matthew Welland.
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
(use regex-case)

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
    (with-output-to-file (conc targdir "/xml/" sheet-name ".xml")
      (lambda ()
	(print (sxml-serializer#serialize-sxml remaining))))
    sheet-name))

(define (sxml->file dat fname)
  (with-output-to-file fname
    (lambda ()
      (print (sxml-serializer#serialize-sxml dat)))))

(define (extract-txtdb dat targdir)
  (let* ((wrkbk       (find-section   dat   'http://www.gnumeric.org/v10.dtd:Workbook))
	 (wrk-rem     (remove-section dat   'http://www.gnumeric.org/v10.dtd:Workbook))
	 (sheets      (find-section   wrkbk 'http://www.gnumeric.org/v10.dtd:Sheets))
	 (sht-rem     (remove-section wrkbk 'http://www.gnumeric.org/v10.dtd:Sheets))
	 (sheet-names (map (lambda (sheet)
			     (sheet->txtdb sheet targdir))
			   sheets)))
    (create-directory (conc targdir "/xml") #t)
    (sxml->file wrk-rem (conc targdir "/xml/workbook.xml"))
    (sxml->file sht-rem (conc targdir "/xml/sheets.xml"))
    (with-output-to-file (conc targdir "/sheet-names.cfg")
      (lambda ()
	(map print sheet-names)))))

(define (read-gnumeric-file fname)
  (if (not (string-match (regexp ".*.gnumeric$") fname))
      (begin
	(print "ERROR: Attempt to import gnumeric file with extention other than .gnumeric")
	(exit))
      (let ((tmpf (create-temporary-file (pathname-strip-directory fname))))
	(system (conc " gunzip > " tmpf " < " fname))
	(let ((res (txtdb:read-gnumeric-xml tmpf)))
	  (delete-file tmpf)
	  res))))

(define (import-gnumeric-file fname targdir)
  (extract-txtdb (read-gnumeric-file fname) targdir))

(define (txtdb-export dbdir fname)
  (let ((sxml-dat (txtdb->sxml dbdir))
	(tmpf     (create-temporary-file (pathname-strip-directory fname))))
    (with-output-to-file tmpf
      (lambda ()
	(print (sxml-serializer#serialize-sxml sxml-dat))))
    (system (conc "gzip " tmpf))
    (file-copy tmpf fname)
    (delete-file tmpf)))

(define (hash-table-reverse-lookup ht val)
  (hash-table-fold ht (lambda (k v res)(if (equal? v val) k res)) #f))

(define (read-dat fname)
  (let ((section-rx (regexp "^\\[(.*)\\]\\s*$"))
	(comment-rx (regexp "^#.*"))          ;; This means a cell name cannot start with #
	(cell-rx    (regexp "^(\\S+) (.*)$")) ;; One space only for the cellname content separator 
	(blank-rx   (regexp "^\\s*$"))
	(inp        (open-input-file fname)))
    (let loop ((inl     (read-line inp))
	       (section #f)
	       (res     '()))
      (if (eof-object? inl)
	  (begin
	    (close-input-port inp)
	    res)
	  (regex-case
	   inl 
	   (comment-rx _          (read-line inp) section res)
	   (blank-rx   _          (read-line inp) section res)
	   (section-rx (x sname)  (loop (read-line inp) 
					sname 
					res))
	   (cell-rx   (x k v)     (loop (read-line inp)
					section
					(cons (list section k v) res)))
	   (else                  (begin
				    (print "ERROR: Unrecognised line in input file " fname ", ignoring it")
				    (loop (read-line inp) section res))))))))
    
(define (txtdb->sxml dbdir)
  (let* ((sht-names (read-file (conc dbdir "/sheet-names.cfg")  read-line))
	 (wrk-rem   (read-file (conc dbdir "/xml/workbook.xml") read-line))
	 (sht-rem   (read-file (conc dbdir "/xml/sheets.xml")   read-line))
	 (sheets    (fold (lambda (sheetname res)
			    (let ((sheetdat (read-dat (conc dbdir "/" sheetname ".dat")))
				  (sht-meta (txtdb:read-gnumeric-xml (conc dbdir "/xml/" sheetname ".xml"))))
			      (cons (cons sht-meta sheetdat)
				    res)))
			  '()
			  sht-names)))
    (append wrk-rem (list sheets))))

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