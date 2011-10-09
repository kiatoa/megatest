(define-inline (key:get-fieldname key)(vector-ref key 0))
(define-inline (key:get-fieldtype key)(vector-ref key 1))

(define-inline (keys->valslots keys) ;; => ?,?,? ....
  (string-intersperse (map (lambda (x) "?") keys) ","))

(define-inline (keys->key/field keys . additional)
  (string-join (map (lambda (k)(conc (key:get-fieldname k) " " (key:get-fieldtype k)))(append keys additional)) ","))

(define-inline (item-list->path itemdat)
  (string-intersperse  (map cadr itemdat) "/"))

