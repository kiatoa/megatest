(use http-client)

(with-input-from-request "http://localhost:12345/hey"
                                           ;; #f
                                           ;; msg 
                                           (list (cons 'dat "Testing eh"))
                                           read-string)

