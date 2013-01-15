(use http-client)

(print (with-input-from-request "http://localhost:8082/?foo=1" #f read-string))