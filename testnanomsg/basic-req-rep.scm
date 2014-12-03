(use nanomsg srfi-18 sqlite3 numbers)

(define resp (nn-socket 'rep))