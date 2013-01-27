(use regex http-client)

(print (with-input-from-request "http://localhost:8083/?foo=1" #f 
    (lambda ()
      (let ((match (string-search (regexp "<body>(.*)<.body>") (caddr (string-split (read-string) "\n")))))
	(cadr match)))))
