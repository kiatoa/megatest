
(use iup canvas-draw canvas-draw-iup)

(define t #f) 

(define tree-dialog
  (dialog
   #:title "Tree Test"
   (let ((t1 (treebox
                 #:selection_cb (lambda (obj id state)
                                    (print "selection_db with id=" id " state=" state)
                                    (print "Userdata: " (attribute obj "USERDATA"))
                                    (attribute-set! obj "USERDATA" "Testvalue")))))
     (set! t t1)
     t1)))

(show tree-dialog)
(map (lambda (elname el)
       (print "Adding " elname " with value " el)
       (attribute-set! t elname el)
       (attribute-set! t "USERDATA" el))
     '("VALUE" "NAME"    "ADDLEAF" "ADDBRANCH1" "ADDLEAF2"    "VALUE")
     '("0"     "Figures" "Other"   "triangle"   "equilateral" "4")
     )
(map (lambda (attr)
       (print attr " is " (attribute t attr)))
     '("KIND1" "PARENT2" "STATE1"))
(main-loop)
