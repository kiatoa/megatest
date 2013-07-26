
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
(use posix)
(use json)
(use csv)

;; Read a non-compressed gnumeric file
(define (refdb:read-gnumeric-xml fname)
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

(define (sheet->refdb dat targdir)
  (let* ((comment-rx  (regexp "^#CMNT\\d+\\s*"))
	 (blank-rx    (regexp "^#BLNK\\d+\\s*"))
	 (sheet-name  (car (find-section dat 'http://www.gnumeric.org/v10.dtd:Name)))
	 ;; (safe-name   (string->safe-filename sheet-name))
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
				  (let ((key (car row))
					(val (cadr row)))
				    (if (string-search comment-rx key)
					(print val)
					(if (string-search blank-rx key)
					    (print)
					    (print key " " val)))))
				(reverse (hash-table-ref cols colname)))
		      ;; (print)
		      )
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
	(pp remaining)))
    sheet-name))

(define (sxml->file dat fname)
  (with-output-to-file fname
    (lambda ()
      ;; (print (sxml-serializer#serialize-sxml dat))
      (pp dat))))

(define (file->sxml fname)
  (let ((res (read-file fname read)))
    (if (null? res)
	(begin
	  (print "ERROR: file " fname " is malformed for read")
	  #f)
	(car res))))

;; Write an sxml gnumeric workbook to a refdb directory structure.
;;
(define (extract-refdb dat targdir)
  (create-directory (conc targdir "/sxml") #t)
  (let* ((wrkbk       (find-section   dat   'http://www.gnumeric.org/v10.dtd:Workbook))
	 (wrk-rem     (remove-section dat   'http://www.gnumeric.org/v10.dtd:Workbook))
	 (sheets      (find-section   wrkbk 'http://www.gnumeric.org/v10.dtd:Sheets))
	 (sht-rem     (remove-section wrkbk 'http://www.gnumeric.org/v10.dtd:Sheets))
	 (sheet-names (map (lambda (sheet)
			     (sheet->refdb sheet targdir))
			   sheets)))
    (sxml->file wrk-rem (conc targdir "/sxml/_workbook.sxml"))
    (sxml->file sht-rem (conc targdir "/sxml/_sheets.sxml"))
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
	(let ((res (refdb:read-gnumeric-xml tmpf)))
	  (delete-file tmpf)
	  res))))

(define (import-gnumeric-file fname targdir)
  (extract-refdb (read-gnumeric-file fname) targdir))

;; Write a gnumeric compressed xml spreadsheet from a refdb directory structure.
;;
(define (refdb-export dbdir fname)
  (let* ((sxml-dat (refdb->sxml dbdir))
	 (tmpf     (create-temporary-file (pathname-strip-directory fname)))
	 (tmpgzf   (conc tmpf ".gz")))
    (with-output-to-file tmpf
      (lambda ()
	(print (sxml-serializer#serialize-sxml sxml-dat ns-prefixes: (list (cons 'gnm "http://www.gnumeric.org/v10.dtd"))))))
    (system (conc "gzip " tmpf))
    (file-copy tmpgzf fname #t)
    (delete-file tmpgzf)))

(define (hash-table-reverse-lookup ht val)
  (hash-table-fold ht (lambda (k v res)(if (equal? v val) k res)) #f))

(define (read-dat fname)
  (let ((section-rx  (regexp "^\\[(.*)\\]\\s*$"))
	(comment-rx  (regexp "^#.*"))          ;; This means a cell name cannot start with #
	(cell-rx     (regexp "^(\\S+) (.*)$")) ;; One space only for the cellname content separator 
	(blank-rx    (regexp "^\\s*$"))
	(inp         (open-input-file fname))
	(cmnt-indx   (make-hash-table))
	(blnk-indx   (make-hash-table)))
    (let loop ((inl     (read-line inp))
	       (section #f)
	       (res     '()))
      (if (eof-object? inl)
	  (begin
	    (close-input-port inp)
	    (reverse res))
	  (regex-case
	   inl 
	   (comment-rx _          (let ((curr-indx (+ 1 (hash-table-ref/default cmnt-indx section 0))))
				    (hash-table-set! cmnt-indx section curr-indx)
				    (loop (read-line inp)
					  section 
					  (cons (list (conc "#CMNT" curr-indx) section inl) res))))
	   (blank-rx   _          (let ((curr-indx (+ 1 (hash-table-ref/default blnk-indx section 0))))
				    (hash-table-set! blnk-indx section curr-indx)
				    (loop (read-line inp)
					  section
					  (cons (list (conc "#BLNK" curr-indx) section " ") res))))
	   (section-rx (x sname)  (loop (read-line inp) 
					sname 
					res))
	   (cell-rx   (x k v)     (loop (read-line inp)
					section
					(cons (list k section v) res)))
	   (else                  (begin
				    (print "ERROR: Unrecognised line in input file " fname ", ignoring it")
				    (loop (read-line inp) section res))))))))

(define (get-value-type val expressions)
  (cond 
   ((string->number val) '(ValueType "40"))
   ((equal? val "")      '(ValueType "60"))
   ((equal? (substring val 0 1) "=")
    (let ((exid (hash-table-ref/default expressions val #f)))
      (if exid 
	  (list 'ExprID exid)
	  (let* ((values  (hash-table-keys expressions)) ;; note, values are the id numbers
		 (new-max (+ 1 (if (null? values) 0 (apply max values)))))
	    (hash-table-set! expressions val new-max)
	    (list 'ExprID new-max)))))
   (else '(ValueType "60"))))

(define (dat->cells dat)
  (let* ((indx     (common:sparse-list-generate-index dat))
	 (row-indx (car indx))
	 (col-indx (cadr indx))
	 (rowdat   (map (lambda (row)(list (car row) "    " (car row))) row-indx))
	 (coldat   (map (lambda (col)(list "    " (car col) (car col))) col-indx))
	 (exprs    (make-hash-table)))
    (list (cons 'http://www.gnumeric.org/v10.dtd:Cells 
		(map (lambda (item)
		       (let* ((row-name (car item))
			      (col-name (cadr item))
			      (row-num  (let ((i (assoc row-name row-indx)))
					  (if i (cadr i) 0))) ;; 0 for the title row/col
			      (col-num  (let ((i (assoc col-name col-indx)))
					  (if i (cadr i) 0)))
			      (value    (caddr item))
			      (val-type (get-value-type value exprs)))
			 (list 'http://www.gnumeric.org/v10.dtd:Cell
			       (list '@ val-type (list 'Row (conc row-num)) (list 'Col (conc col-num)))
			       value)))
		     (append rowdat coldat dat))))))
    
(define (refdb->sxml dbdir)
  (let* ((sht-names (read-file (conc dbdir "/sheet-names.cfg")  read-line))
	 (wrk-rem   (file->sxml (conc dbdir "/sxml/_workbook.sxml")))
	 (sht-rem   (file->sxml (conc dbdir "/sxml/_sheets.sxml")))
	 (sheets    (fold (lambda (sheetname res)
			    (let* ((sheetdat (read-dat (conc dbdir "/" sheetname ".dat")))
				   (cells    (dat->cells sheetdat))
				   (sht-meta (file->sxml (conc dbdir "/sxml/" sheetname ".sxml"))))
			      (cons (cons (car sht-meta) 
					  (append (cons (list 'http://www.gnumeric.org/v10.dtd:Name sheetname)
							(cdr sht-meta))
						  cells))
				    res)))
			  '()
			 (reverse  sht-names))))
    (append wrk-rem (list (append
			   (cons 'http://www.gnumeric.org/v10.dtd:Workbook
				 sht-rem)
			   (list (cons 'http://www.gnumeric.org/v10.dtd:Sheets sheets)))))))

;; (define (

;; 
;; optional apply proc to rownum colnum value
;; 
;; NB// If a change is made to this routine please look also at applying
;;      it to the code in Megatest (http://www.kiatoa.com/fossils/megatest)
;;      in the file common.scm
;;
(define (common:sparse-list-generate-index data #!key (proc #f))
  (if (null? data)
      (list '() '())
      (let loop ((hed (car data))
		 (tal (cdr data))
		 (rownames '())
		 (colnames '())
		 (rownum   0)
		 (colnum   0))
	(let* ((rowkey          (car   hed))
	       (colkey          (cadr  hed))
	       (value           (caddr hed))
	       (existing-rowdat (assoc rowkey rownames))
	       (existing-coldat (assoc colkey colnames))
	       (curr-rownum     (if existing-rowdat rownum (+ rownum 1)))
	       (curr-colnum     (if existing-coldat colnum (+ colnum 1)))
	       (new-rownames    (if existing-rowdat rownames (cons (list rowkey curr-rownum) rownames)))
	       (new-colnames    (if existing-coldat colnames (cons (list colkey curr-colnum) colnames))))
	  ;; (debug:print-info 0 "Processing record: " hed )
	  (if proc (proc curr-rownum curr-colnum rowkey colkey value))
	  (if (null? tal)
	      (list new-rownames new-colnames)
	      (loop (car tal)
		    (cdr tal)
		    new-rownames
		    new-colnames
		    (if (> curr-rownum rownum) curr-rownum rownum)
		    (if (> curr-colnum colnum) curr-colnum colnum)
		    ))))))
(define help
  (conc "Usage: refdb action params ...

Note: refdbdir is a path to the directory containg sheet-names.cfg

  import filename.gnumeric refdbdir   : Import a gnumeric file into a txt db directory
  edit   refdbdir                     : Edit a refdbdir using gnumeric.
  ls refdbdir                         : List the keys for specified level 
  lookup refdbdir sheetname row col   : Look up a value in the text db   
  getrownames refdb sheetname         : Get a list of row titles
  getcolnames refdb sheetname         : Get a list of column titles  

Part of the Megatest tool suite. Learn more at http://www.kiatoa.com/fossils/megatest"))

(define (list-sheets path)
  ;; (cond
  ;;  ((and path (not sheet)(not row)(not col))
  (if (file-exists? path)
      (read-file (conc path "/sheet-names.cfg") read-line)
      '()))
;; ((and path sheet (not row)(not col))

(define (lookup path sheet row col)
  (let ((fname (conc path "/" sheet ".dat")))
    (if (file-exists? fname)
	(let ((dat (read-dat fname)))
	  (if (null? dat)
	      #f
	      (let loop ((hed (car dat))
			 (tal (cdr dat)))
		(if (and (equal? row (car hed))
			 (equal? col (cadr hed)))
		    (caddr hed)
		    (if (null? tal)
			#f
			(loop (car tal)(cdr tal)))))))
	#f)))

;; call with proc = car to get row names
;; call with proc = cadr to get col names
(define (get-rowcol-names path sheet proc)
  (let ((fname (conc path "/" sheet ".dat"))
	(cmnt-rx (regexp "^#CMNT\\d+\\s*"))
	(blnk-rx (regexp "^#BLNK\\d+\\s*")))
    (if (file-exists? fname)
	(let ((dat (read-dat fname)))
	  (if (null? dat)
	      '()
	      (let loop ((hed (car dat))
			 (tal (cdr dat))
			 (res '()))
		(let* ((row-name (proc hed))
		       (newres (if (and (not (member row-name res))
					(not (string-search cmnt-rx row-name))
					(not (string-search blnk-rx row-name)))
				   (cons row-name res)
				   res)))
		  (if (null? tal)
		      (reverse newres)
		      (loop (car tal)(cdr tal) newres))))))
	'())))

;; (define (get-col-names path sheet)
;;   (let ((fname (conc path "/" sheet ".dat")))
;;     (if (file-exists? fname)
;; 	(let ((dat (read-dat fname)))
;; 	  (if (null? dat)
;; 	      #f
;; 	      (map cadr dat))))))

(define (edit-refdb path)
  ;; TEMPORARY, REMOVE IN 2014
  (if (not (file-exists? (conc path "/sxml/_sheets.sxml")))
      (begin
	(print "ERROR: You appear to have the old file structure for txtdb. Please do the following and try again.")
	(print)
	(print "mv " path "/sxml/sheets.sxml " path "/sxml/_sheets.sxml")
	(print "mv " path "/sxml/workbook.sxml " path "/sxml/_workbook.sxml")
	(print)
	(print "Don't forget to remove the old files from your revision control system and add the new.")
	(exit)))
  (let* ((dbname  (pathname-strip-directory path))
	 (tmpf    (conc (create-temporary-file dbname) ".gnumeric")))
    (if (file-exists? (conc path "/sheet-names.cfg"))
	(refdb-export path tmpf))
    (let ((pid (process-run "gnumeric" (list tmpf))))
      (process-wait pid)
      (import-gnumeric-file tmpf path))))

(define (process-action action-str . param)
  (let ((num-params (length param))
	(action     (string->symbol action-str)))
    (cond
     ((eq? num-params 1)
      (case action
	((edit)
	 (edit-refdb (car param)))
	((ls)
	 (map print (list-sheets (car param))))))
     ((eq? num-params 2)
      (case action
	((getrownames)(print (string-intersperse (get-rowcol-names (car param)(cadr param) car)  " ")))
	((getcolnames)(print (string-intersperse (get-rowcol-names (car param)(cadr param) cadr) " ")))
	((import)
	 (let ((fname     (car param))
	       (targname  (cadr param)))
	   (import-gnumeric-file fname targname)))))
     ((eq? num-params 4)
      (case action
	((lookup)               ;; path    section     row          col 
	 (let ((res (lookup (car param)(cadr param)(caddr param)(cadddr param))))
	   (if res 
	       (print res)
	       (begin
		 (print "")
		 (exit 1))))))))))

(define (main)
  (let* ((args (argv))
	 (prog (car args))
	 (rema (cdr args)))
    (cond
     ((null? rema)(print help))
     ((>= (length rema) 2)
      (apply process-action (car rema)(cdr rema)))
     (else (print help)))))

;;======================================================================
;;  C R E A T E   N E W   D B S
;;======================================================================

(include "txtdb/metadat.scm")

;; Creates a new db at path with one sheet
(define (create-new-db path)
  (create-directory path #t)
  (create-directory (conc path "/sxml") #t))
  ;; Sheet1.dat
  ;; sheet-names.cfg
  ;; sxml/Sheet1.sxml
  ;; sxml/_sheets.sxml
  ;; sxml/_workbook.sxml


(main)

#|  
 (define x (refdb:read-gnumeric-xml "testdata-stripped.xml"))



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