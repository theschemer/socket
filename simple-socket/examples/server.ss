(import (simple-socket socket-ffi))

(define socket-fd (socket AF_INET SOCK_STREAM IPPROTO_IP))
(define port 8245)
(bind socket-fd AF_INET "127.0.0.1" port)
(listen socket-fd 10)
(printf "listen on ~a\n" port)
(define clinet-fd (accept socket-fd))

(let* ([s "Hello World!"]
       [bv (string->utf8 s)])
  (c-write clinet-fd bv 0 (bytevector-length bv)))

(close clinet-fd)
(close socket-fd)


