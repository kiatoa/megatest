(define-inline (debug:print n . params)
  (if (<= n *verbosity*)
      (apply print params)))

;; if a value is printable (i.e. string or number) return the value
;; else return an empty string
(define-inline (printable val)
  (if (or (number? val)(string? val)) val ""))

