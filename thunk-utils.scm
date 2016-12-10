(use srfi-18)


;; wrap a proc with a mutex so that two threads may not call proc simultaneously.
;; will catch exceptions to ensure mutex is unlocked even if exception is thrown.
;; will generate a unique mutex for proc unless one is specified with canned-mutex: option
;;
;; example 1: (define thread-safe-+ (make-synchronized-proc +))
;; example 2: (define thread-safe-plus
;;               (make-synchronized-proc
;;                  (lambda (x y)
;;                      (+ x y))))

(define (make-synchronized-proc proc
                                #!key (canned-mutex #f))
  (let* ((guard-mutex (if canned-mutex canned-mutex (make-mutex)))
         (guarded-proc ;; we are guarding the thunk against exceptions.  We will record whether result of evaluation is an exception or a regular result.
          (lambda args
            (mutex-lock! guard-mutex)
            (let* ((EXCEPTION (gensym)) ;; using gensym to avoid potential collision with a proc that returns a pair having the first element be our flag.  gensym guarantees the symbol is unique.
                   (res
                    (condition-case
                     (apply proc args) ;; this is what we are guarding the execution of
                     [x () (cons EXCEPTION x)]
                     )))
              (mutex-unlock! guard-mutex)
              (cond
               ((and (pair? res) (eq? (car res) EXCEPTION))
                (raise (cdr res)))
               (else
                res))))))
    guarded-proc))


;; retry an operation (depends on srfi-18)
;; ==================
;; idea here is to avoid spending time on coding retrying something.  Trying to be generic here.
;;
;; Exception handling:
;; -------------------
;; if evaluating the thunk results in exception, it will be retried.
;; on last try, if final-failure-returns-actual is true, the exception will be re-thrown to caller.
;;
;; look at options below #!key to see how to configure behavior
;;
;;

(define (retry-thunk
         the-thunk
         #!key ;;;; options below
         (accept-result?   (lambda (x) x)) ;; retry if predicate applied to thunk's result is false 
         (retries                       4) ;; how many tries
         (failure-value                #f) ;; return this on final failure, unless following option is enabled:
         (final-failure-returns-actual #f) ;; on failure, on the last try, just return the result, not failure-value

         (retry-delay                 0.1) ;; delay between tries
         (back-off-factor               1) ;; multiply retry-delay by this factor on retry
         (random-delay                0.1) ;; add a random portion of this value to wait

         (chatty                       #f) ;; print status as we go, for debugging.
         )
  
  (when chatty (print) (print "Entered retry-thunk") (print "-=-=-=-=-=-"))
  (let* ((guarded-thunk ;; we are guarding the thunk against exceptions.  We will record whether result of evaluation is an exception or a regular result.
          (lambda ()
           (let* ((EXCEPTION (gensym)) ;; using gensym to avoid potential collision
                  (res
                   (condition-case
                    (the-thunk) ;; this is what we are guarding the execution of
                    [x () (cons EXCEPTION x)]
                    )))
             (cond
              ((and (pair? res) (eq? (car res) EXCEPTION))
               (if chatty
                   (print " - the-thunk threw exception >"(cdr res)"<"))
               (cons 'exception (cdr res)))
               (else
                (if chatty
                    (print " - the-thunk returned result >"res"<"))
                (cons 'regular-result res)))))))
    
    (let loop ((guarded-res (guarded-thunk))
               (retries-left retries)
               (fail-wait retry-delay))
      (if chatty (print "   =========="))
      (let* ((wait-time (+ fail-wait (+ (* fail-wait back-off-factor)
                                        (* random-delay
                                           (/ (random 1024) 1024) ))))
             (res-type (car guarded-res))
             (res-value (cdr guarded-res)))
        (cond
         ((and (eq? res-type 'regular-result) (accept-result? res-value))
                   (if chatty (print " + return result that satisfied accept-result? >"res-value"<"))
                   res-value)

         ((> retries-left 0)
          (if chatty (print " - sleep "wait-time))
          (thread-sleep! wait-time)
          (if chatty (print " + retry ["retries-left" tries left]"))
          (loop (guarded-thunk)
                (sub1 retries-left)
                wait-time))
         
         ((eq? res-type 'regular-result)
          (if final-failure-returns-actual
              (begin
                (if chatty (print " + last try failed- return the result >"res-value"<"))
                res-value)
              (begin
                (if chatty (print " + last try failed- return canned failure value >"failure-value"<"))
              failure-value)))
         
         (else ;; no retries left; result was not accepted and res-type can only be 'exception
          (if final-failure-returns-actual 
              (begin
                (if chatty (print " + last try failed with exception- re-throw it >"res-value"<"))
                (abort res-value)); re-raise the exception. TODO: find a way for call-history to show as though from entry to this function
              (begin
                (if chatty (print " + last try failed with exception- return canned failure value >"failure-value"<"))
                failure-value))))))))

