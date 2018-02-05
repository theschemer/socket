(import (simple-socket socket-ffi))

(define socket-fd 0)
(set! socket-fd (socket AF_INET SOCK_STREAM IPPROTO_IP))
(connect socket-fd AF_INET "127.0.0.1" 8200)

(define str  (make-string 1024))
(c-read socket-fd str  (string-length str))
(printf "yessss---------~a" str)
(close socket-fd)