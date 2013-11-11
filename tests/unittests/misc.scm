;;======================================================================
;; P R O C E S S E S
;;======================================================================

(test "cmd-run-with-stderr->list" '("No such file or directory")
      (let ((reslst (cmd-run-with-stderr->list "ls" "/tmp/ihadbetternotexist")))
	(string-search (regexp "No such file or directory")(car reslst))))

;;======================================================================
;; T E S T   M A T C H I N G
;;======================================================================

;; tests:glob-like-match
(test #f '("abc") (tests:glob-like-match "abc" "abc"))
(for-each 
 (lambda (patt str expected)
   (test (conc patt " " str "=>" expected) expected (tests:glob-like-match patt str)))
 (list "abc"    "~abc" "~abc" "a*c"  "a%c")
 (list "abc"    "abcd" "abc"  "ABC"  "ABC")
 (list '("abc")  #t      #f     #f '("ABC"))
 )

;; tests:match
(test #f #t (tests:match "abc/def" "abc" "def"))
(for-each 
 (lambda (patterns testname itempath expected)
   (test (conc patterns " " testname "/" itempath "=>" expected)
	 expected 
	 (tests:match patterns testname itempath)))
 (list "abc" "abc/%" "ab%/c%" "~abc/c%" "abc/~c%" "a,b/c,%/d" "%/,%/a" "%/,%/a" "%/,%/a" "%" "%" "%/" "%/")
 (list "abc" "abc"   "abcd"   "abc"     "abc"     "a"         "abc"     "def"    "ghi"   "a" "a"  "a"  "a")
 (list   ""  ""      "cde"    "cde"     "cde"     ""            ""      "a"       "b"    ""  "b"  ""   "b")
 (list   #t    #t       #t    #f           #f      #t           #t       #t       #f     #t  #t   #t    #f))

;; db:patt->like
(test #f "testname LIKE 't%'" (db:patt->like "testname" "t%" comparator: " AND "))
(test #f "testname LIKE 't%' AND testname LIKE '%t'" (db:patt->like "testname" "t%,%t" comparator: " AND "))
(test #f "item_path GLOB ''" (db:patt->like "item_path" ""))

;; test:match->sqlqry
(test #f "(testname GLOB 'a' AND item_path GLOB 'b') OR (testname LIKE 'a%' AND item_path LIKE '%') OR (testname GLOB '' AND item_path LIKE 'b%')"
      (tests:match->sqlqry "a/b,a%,/b%"))
(test #f "(testname GLOB 'a' AND item_path GLOB 'b') OR (testname LIKE 'a%' AND item_path LIKE '%') OR (testname LIKE '%' AND item_path LIKE 'b%')"
      (tests:match->sqlqry "a/b,a%,%/b%"))

