
(use extras)
(use data-structures)
(use srfi-1)
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

(define (tests:get-test-list test-registry)
  (map car test-registry))


(define (alist-push alist key val)
  (let ((current (alist-ref key alist)))
    (if current
        (alist-update key (cons val current) alist)
        (cons (list key val) alist))))

  
(define (test:get-adj-list test-registry)
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
   ((member '% items) (print "% in items")
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


(define (testpatt->alist testpatt)
  (if (string? testpatt)
      (let ((patts (string-split testpatt ",")))
        (if (null? patts) ;;; no pattern(s) means no match
            #f
            (let loop ((rest-patts patts) (res  '()))
              ;; (print "loop: patt: " patt ", tal " tal)
              (if (null? rest-patts)
                  res
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
                    (print "BB->: test="test" item-patt-raw="item-patt-raw" item-patt="item-patt" existing-item-patts="existing-item-patts" new-item-patts="new-item-patts)
                    (loop tal-rest-patts new-res))))))))

(define (traverse node adj-list path)
  ;(print "node="node" path="path)
  (let ((children (alist-ref node adj-list)))
    (cond
     ((not children)  (list (cons node path)))
     (else
      (apply append
             (map
              (lambda (child)
                (traverse child adj-list (cons node path)))
              children))))))

(define test-registry
  '(
    (aa . ( (items . ( 1 2 3 )) ))
    (a  . ( (items . ( 1 2 3 )) ))
    (b  . ( (items . ( 1 2 3 ))
           (waitons . (a)   ) ) )
    (c  . ( (items . ( 1 2 3 ))
           (waitons . (a)   ) ) )
    (f  . ( (items . ( 1 2 3 ))
           (waitons . (a)   ) ) )
    (d  . ( (items . ( 1 2 3 ))
           (waitons . (b c) ) ) )
    (g  . ( (items . ( 1 2 3 ))
           (waitons . (b)   ) ) )
    (e  . ( (items . ( 1 2 3 ))
           (waitons . (d)   ) ) )
    (h  . ( (items . ( 1 2 3 ))
           (waitons . (d)   ) ) )
       ))

(set! test-registry2
      (cons
       (cons 'ALL-TESTS (list (cons 'waitons (tests:get-test-list test-registry))))
       test-registry))



(pretty-print test-registry)
(define adj-list (test:get-adj-list test-registry))

(print "adjacency list=")(pretty-print adj-list)

(print "topological-sort=" (topological-sort adj-list eq?))

(define seed-testpatt "a/1,a/2,d,aa/%")
(define seed-testpatt-alist (testpatt->alist seed-testpatt))

;;(define seed-tests '(d aa))
(define seed-tests (map car seed-testpatt-alist))
(print "seed-testpatt="seed-testpatt"\n** seed-testpatt-alist="seed-testpatt-alist"\n seed-tests="seed-tests)

(define waiton-paths
  (map
   reverse
   (apply append
          (map
           (lambda (test)
             (traverse test adj-list '())) seed-tests))))
       

(print "waiton-paths=")
(pretty-print waiton-paths)  


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
               
(print "testpatts from first path="(car waiton-paths))

(define (condense-alist alist)
  (let loop ((rest-alist alist) (res '()))
    (if (null? rest-alist)
        res
        (let* ((hed-alist (car rest-alist))
               (tal-alist (cdr rest-alist))
               (key (car hed-alist))
               (new-items (cdr hed-alist))
               (existing-list (alist-ref key res))
               (new-list
                (if existing-list
                    (append-items-lists new-items existing-list)
                    new-items
                    ))
               (new-res (alist-update key new-list res)))
          (loop tal-alist new-res)))))
               
                    

(define (get-elaborated-testpatt-alist waiton-paths seed-testpatt-alist test-registry)
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
    (condense-alist raw-res)))
        


(pretty-print
 (get-elaborated-testpatt-alist waiton-paths seed-testpatt-alist test-registry))










  

