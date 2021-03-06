;;======================================================================
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

;; make-vector-record tasks task id action owner state target name test item params creation_time execution_time 
(define (make-tasks:task)(make-vector 11))
(define-inline (tasks:task-get-id               vec)    (vector-ref  vec 0))
(define-inline (tasks:task-get-action           vec)    (vector-ref  vec 1))
(define-inline (tasks:task-get-owner            vec)    (vector-ref  vec 2))
(define-inline (tasks:task-get-state            vec)    (vector-ref  vec 3))
(define-inline (tasks:task-get-target           vec)    (vector-ref  vec 4))
(define-inline (tasks:task-get-name             vec)    (vector-ref  vec 5))
(define-inline (tasks:task-get-testpatt         vec)    (vector-ref  vec 6))
(define-inline (tasks:task-get-keylock          vec)    (vector-ref  vec 7))
(define-inline (tasks:task-get-params           vec)    (vector-ref  vec 8))
(define-inline (tasks:task-get-creation_time    vec)    (vector-ref  vec 9))
(define-inline (tasks:task-get-execution_time   vec)    (vector-ref  vec 10))

(define-inline (tasks:task-set-state!  vec val)(vector-set! vec 3 val))


;; make-vector-record tasks monitor id pid start_time last_update hostname username
(define (make-tasks:monitor)(make-vector 5))
(define-inline (tasks:monitor-get-id            vec)    (vector-ref  vec 0))
(define-inline (tasks:monitor-get-pid           vec)    (vector-ref  vec 1))
(define-inline (tasks:monitor-get-start_time    vec)    (vector-ref  vec 2))
(define-inline (tasks:monitor-get-last_update   vec)    (vector-ref  vec 3))
(define-inline (tasks:monitor-get-hostname      vec)    (vector-ref  vec 4))
(define-inline (tasks:monitor-get-username      vec)    (vector-ref  vec 5))
