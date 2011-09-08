(load "ods.scm")

(ods:list->ods 
 "testing"
 "testing.ods"
  '((Sheet1 ("Row 1,A" "Row 1,B")
 	   ("Row 2,A" "Row 2,B"))
    (Sheet2 (1 2)
	    (3 4)
	    ()
	    ("This is sheet 2"))
    (Sheet_3 ("Test" "Item Path" "Category" "Value" "Comment")
	     ("LVS_esd" "eb8zxffd" "Cells"  "n"))))
