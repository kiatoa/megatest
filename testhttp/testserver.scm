(use spiffy uri-common intarweb spiffy-request-vars)

(root-path "/var/www")

(vhost-map `(((* any) . ,(lambda (continue)
                           (let (($ (request-vars source: 'both)))
                           (print ($ 'dat))
                           (if (equal? (uri-path (request-uri (current-request))) 
                                       '(/ "hey"))
                               (send-response body: "hey there!\n"
                                              headers: '((content-type text/plain)))
                               (continue)))))))

(start-server port: 12345)


