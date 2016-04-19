
(use extras)
(use data-structures)
(use srfi-1)
(use srfi-13)
(use regex)


(define (tests:get-test-property test-registry test property)
  (let loop ((rem-test-registry test-registry) (res #f))
    (if (null? rem-test-registry)
        res
        (let* ((this-test (car rem-test-registry))
              (this-testname (car this-test))
              (this-testrec (cdr this-test)))
          (if (eq? this-testname test)
              (alist-ref property this-testrec)
              (loop (cdr rem-test-registry) res))))))

(define (tests:get-test-waitons test-registry test)
  (tests:get-test-property test-registry test 'waitons))

(define (tests:get-test-items test-registry test)
  (or (tests:get-test-property test-registry test 'items) '()))

(define (tests:get-test-list test-registry)
  (map car test-registry))


(define (alist-push alist key val)
  (let ((current (alist-ref key alist)))
    (if current
        (alist-update key (cons val current) alist)
        (cons (list key val) alist))))

  
(define (test:get-adjacency-list test-registry)
  (let loop ((rem-tests (tests:get-test-list test-registry)) (res '()))
    (if (null? rem-tests)
        res
        (let* ((test (car rem-tests))
               (rest-rem-tests (cdr rem-tests))
               (waitons
                (or
                 (tests:get-test-waitons test-registry test)
                 '())))
          (loop rest-rem-tests
                (let loop2 ((rem-waitons waitons) (res2 res))
                  (if (null? rem-waitons)
                      res2
                      (let* ((waiton (car rem-waitons))
                             (rest-waitons (cdr rem-waitons))
                             (next-res (alist-push res2 waiton test)))
                        (loop2 rest-waitons next-res)))))))))



(define (add-item-to-items-list item items)
  (cond
   ((eq? item '%) 
    (list '%))
   ((member '% items) 
    (list '%))
   ((member item items) 
    items)
   (else
    (cons item items))))

(define (append-items-lists l1 l2)
  (let loop ((rem-l1 l1) (res l2))
    (if (null? rem-l1)
        res
        (let* ((hed-rem-l1 (car rem-l1))
               (tal-rem-l1 (cdr rem-l1))
               (new-res (add-item-to-items-list hed-rem-l1 res)))
          (loop tal-rem-l1 new-res)))))

(define (sort-any inlist)
  (sort inlist (lambda (x y) (string<? (->string x) (->string y)))))

(define (condense-itemlist itemlist test test-registry)
  (let* ((test-items (tests:get-test-items test-registry test))
         (sorted-test-items (sort-any test-items))
         (sorted-itemlist (sort-any itemlist))
         (res
          (cond
           ((member '% itemlist )      '(%))
           ((equal? sorted-itemlist sorted-test-items)   '(%))
           (else
            (sort-any itemlist)))))
    ;;(print "condense-itemlist test-items="sorted-test-items" itemlist="sorted-itemlist"   res="res "   [equal?="(equal? sorted-test-items sorted-itemlist)"]")
    res))


; TODO : warning if itempatt matches no items in test in test registry
(define (condense-testpatt-alist-dummy alist test-registry) alist)
(define (condense-testpatt-alist alist test-registry)
  (let loop ((rest-alist alist) (res '()))
    (if (null? rest-alist)
        res
        (let* ((hed-alist (car rest-alist))
               (tal-alist (cdr rest-alist))
               (testname (car hed-alist))
               (incoming-items (cdr hed-alist))
               (existing-item-list (alist-ref testname res))
               (new-items-list
                (condense-itemlist
                 (if existing-item-list
                     (append-items-lists incoming-items existing-item-list)
                     incoming-items)
                 testname
                 test-registry))
               (new-res (alist-update testname new-items-list res)))
          (loop tal-alist new-res)))))
               

; TODO : warning if itempatt matches no items in test in test registry
(define (testpatt:alist->string testpatt-alist test-registry)
  (string-join
   (let loop ((rem-alist testpatt-alist) (res '()))
     (if (null? rem-alist)
        res
        (let* ((hed-alist (car rem-alist))
               (tal-alist (cdr rem-alist))
               (testname (car hed-alist))
               (item-patts (cdr hed-alist))
               (test-patts
                (map
                 (lambda (item)
                   (conc (->string testname) "/" (->string item)))
                 item-patts))
               (this-res (string-join test-patts ","))
               (new-res (cons this-res res)))
          ;;(print "bb: test-patts="test-patts" this-res="this-res" res="res" new-res="new-res)
          (loop tal-alist new-res))))
   ","))
                            
                         

               
          
  
(define (testpatt:string->alist testpatt test-registry)
  (if (string? testpatt)
      (let ((patts (string-split testpatt ",")))
        (if (null? patts) ;;; no pattern(s) means no match
            #f
            (let loop ((rest-patts patts) (res  '()))
              ;; (print "loop: patt: " patt ", tal " tal)
              (if (null? rest-patts)
                  (condense-testpatt-alist res test-registry)
                  (let* ((hed-patt (car rest-patts))
                         (tal-rest-patts (cdr rest-patts))
                         (patt-parts (string-match (regexp "^([^\\/]*)(\\/(.*)|)$") hed-patt))
                         (test (string->symbol (cadr patt-parts)))
                         (item-patt-raw  (cadddr patt-parts))
                         (item-patt
                          (if item-patt-raw
                              (string->symbol item-patt-raw)
                              '%))
                         (existing-item-patts (or (alist-ref test res) '()))
                         (new-item-patts (add-item-to-items-list item-patt existing-item-patts))
                         (new-res (alist-update test new-item-patts res)))
                    ;(print "BB->: test="test" item-patt-raw="item-patt-raw" item-patt="item-patt" existing-item-patts="existing-item-patts" new-item-patts="new-item-patts)
                    (loop tal-rest-patts new-res))))))))

(define (traverse node adjacency-list path)
  ;(print "node="node" path="path)
  (let ((children (alist-ref node adjacency-list)))
    (cond
     ((not children)  (list (cons node path)))
     (else
      (apply append
             (map
              (lambda (child)
                (traverse child adjacency-list (cons node path)))
              children))))))






(define (get-waiton-items parent-test parent-item-patterns waiton-test test-registry)
  (let* ((parent-item->waiton-item (lambda (x) x)) ;; super simplified vs. megatest, should use itemmap property
         (waiton-test-items (or (tests:get-test-property test-registry waiton-test 'items) '(%)))
         )
    (let loop ((rest-parent-item-patterns parent-item-patterns) (res '()))
      (if (null? rest-parent-item-patterns)
          res
          (let* ((hed-parent-item (car rest-parent-item-patterns))
                 (tal-parent-items (cdr rest-parent-item-patterns))
                 (newres (add-item-to-items-list (parent-item->waiton-item hed-parent-item) res)))
            (loop tal-parent-items newres))))))

;; TODO: only do this with itemwait set true, not always
(define (push-itempatt-down-path waiton-path seed-items test-registry )
  (let loop ((rest-path waiton-path) (waiton-items seed-items) (res '())  )
    (if (null? rest-path)
        res
        (let* ((hed-test (car rest-path))
               (tal-path (cdr rest-path))
               (waiton-test (car rest-path))
               (waiton-items (get-waiton-items hed-test waiton-items waiton-test test-registry))
               (new-res (cons (cons waiton-test waiton-items) res)))
                 
          (loop tal-path waiton-items new-res)))))
               

(define (sort-testpatt-alist-topologically testpatt-alist toposorted-testlist)
  (let loop ((rem-testlist toposorted-testlist) (res '()))
    (if (null? rem-testlist)
        res   ;; TODO - handle if alist has testsnot in toposort-list
        (let* ((hed-test (car rem-testlist))
               (tal-testlist (cdr rem-testlist))
               (next-res (cons hed-test (alist-ref hed-test testpatt-alist)))
               (new-res (cons next-res res)))
          ;;(print "bb stat - hed-test="hed-test" next-res="next-res)
          (loop tal-testlist new-res)))))
               
  
(define (get-elaborated-testpatt-alist waiton-paths seed-testpatt-alist test-registry toposorted-testlist)
  (let ((raw-res
         (let loop ((rest-waiton-paths waiton-paths) (res '()))
           (if (null? rest-waiton-paths)
               res
               (let* ((hed-path (car rest-waiton-paths))
                      (tal-paths (cdr rest-waiton-paths))
                      (test (car hed-path))
                      (items (alist-ref test seed-testpatt-alist))
                      (new-res (cons (push-itempatt-down-path hed-path items test-registry) res))
                      
                      
                      )
                 (loop tal-paths new-res))))))
    (sort-testpatt-alist-topologically (condense-testpatt-alist (apply append raw-res) test-registry) toposorted-testlist)))


(define (get-elaborated-testpatt seed-testpatt test-registry)
  (let* ((adjacency-list (test:get-adjacency-list test-registry))
         (toposorted-testlist (topological-sort adjacency-list eq?))
         (seed-testpatt-alist (testpatt:string->alist seed-testpatt test-registry))
         (seed-tests (map car seed-testpatt-alist))
         (waiton-paths
          (map reverse
               (apply append
                      (map
                       (lambda (test)
                         (traverse test adjacency-list '())) seed-tests))))
         (final-testpatt-alist
          (get-elaborated-testpatt-alist
           waiton-paths
           seed-testpatt-alist
           test-registry
           toposorted-testlist))
         (final-testpatt-string (testpatt:alist->string final-testpatt-alist test-registry)))
    final-testpatt-string))




;; (set! test-registry2
;;       (cons
;;        (cons 'ALL-TESTS (list (cons 'waitons (tests:get-test-list test-registry))))
;;        test-registry))

(define test-registry
  '(
    (aa . ( (items . ( i1 i2 i3 )) ))
    (a  . ( (items . ( i1 i2 i3 )) ))
    (b  . ( (items . ( i1 i2 i3 ))
           (waitons . (a)   ) ) )
    (c  . ( (items . ( i1 i2 i3 ))
           (waitons . (a)   ) ) )
    (f  . ( (items . ( i1 i2 i3 ))
           (waitons . (a)   ) ) )
    (d  . ( (items . ( i1 i2 i3 ))
           (waitons . (b c) ) ) )
    (g  . ( (items . ( i1 i2 i3 ))
           (waitons . (b)   ) ) )
    (e  . ( (items . ( i1 i2 i3 ))
           (waitons . (d)   ) ) )
    (h  . ( (items . ( i1 i2 i3 ))
           (waitons . (d)   ) ) )
       ))

(define seed-testpatt "a/i1,a/i3,d,aa/%")

(define elaborated-testpatt (get-elaborated-testpatt seed-testpatt test-registry))
(print "test-registry:       ")
(pretty-print test-registry)

(print "\nseed-testpatt       = "seed-testpatt)
(print "\nelaborated-testpatt = "elaborated-testpatt)

  

