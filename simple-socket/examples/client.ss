(import (simple-socket socket-ffi))

(define socket-fd (socket AF_INET SOCK_STREAM IPPROTO_IP))
(define port 8245)
(connect socket-fd AF_INET "127.0.0.1" port)

(define buff (make-bytevector 1024))
(let* ([n (c-read socket-fd buff 0 (bytevector-length buff))]
       [bv (make-bytevector n)])
  (bytevector-copy! buff 0 bv 0 n)
  (printf "client: ~a\n" (utf8->string bv)))

(close socket-fd)
(cleanup)