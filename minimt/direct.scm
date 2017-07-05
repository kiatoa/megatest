;; direct API, call the db calls directly
(define rmt:create-run            (statwrap 'create-run  create-run))
(define rmt:create-step           (statwrap 'create-step create-step))
(define rmt:create-test           (statwrap 'create-test create-test))
(define rmt:get-test-id           (statwrap 'get-test-id get-test-id))
(define rmt:get-run-id            (statwrap 'get-run-id  get-run-id))
(define rmt:open-create-db        (statwrap 'open        open-create-db))
(define rmt:step-set-state-status (statwrap 'step-set-state-status step-set-state-status))
(define rmt:test-set-state-status (statwrap 'test-set-state-status test-set-state-status))
(define rmt:test-get-tests        (statwrap 'test-get-tests        test-get-tests))

