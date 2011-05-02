
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; (define (celsius->fahrenheit item)
;;   (let ((number (string->number item)))
;;     (if (number? number)
;; 	(+ (* number 9/5) 32)
;; 	0.0)))

;; (define (megatest-gui-1)
;;   (use pstk)
;;   (handle-exceptions
;;    exn
;;    (tk-end)  ; make sure tk is closed in event of any error
;; 
;;    (tk-start)
;;    (tk/wm 'title tk "Celsius to Fahrenheit")
;;    (let* ((celsius (tk 'create-widget 'entry))
;; 	  (label (tk 'create-widget 'label))
;; 	  (button (tk 'create-widget 'button
;; 		      'text: 'Calculate
;; 		      'command: (lambda () 
;; 				  (label 'configure 
;; 					 'text: (number->string (celsius->fahrenheit (celsius 'get))))))))
;; 					; layout widgets in a grid
;;      (tk/grid celsius 'column: 2 'row: 1 'sticky: 'we 'padx: 5 'pady: 5)
;;      (tk/grid label 'column: 2 'row: 2 'sticky: 'we 'padx: 5 'pady: 5)
;;      (tk/grid button 'column: 2 'row: 3 'sticky: 'we 'padx: 5 'pady: 5)
;;      (tk/grid (tk 'create-widget 'label 'text: "celsius") 
;; 	      'column: 3 'row: 1 'sticky: 'w 'padx: 5 'pady: 5)
;;      (tk/grid (tk 'create-widget 'label 'text: "is") 
;; 	      'column: 1 'row: 2 'sticky: 'e 'padx: 5 'pady: 5)
;;      (tk/grid (tk 'create-widget 'label 'text: "fahrenheit") 
;; 	      'column: 3 'row: 2 'sticky: 'w 'padx: 5 'pady: 5) 					; begin program
;; 					; rest of  gui setup
;;      (tk-event-loop))
;;    ))

(define (init-dialog)
  ;; (let ((controls-frame (iup:frame
  ;;			 (iup:hbox
  #t)			  
  
;; For now the gui work will be done in dashboard.scm

;;(define (megatest-gui)
;;  (require-library iup)
;;  (import (prefix iup iup:))
;;  (use canvas-draw canvas-draw-iup)
;;  (use srfi-4))
  
