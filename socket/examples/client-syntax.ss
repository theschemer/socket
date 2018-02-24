(import (socket socket))

(define port 8211)
(define ip "127.0.0.1")
(define family AF_INET)

(define socket-fd (socket:socket AF_INET SOCK_STREAM IPPROTO_IP))
(socket:connect socket-fd family ip port)
(define buff (socket:read socket-fd))

(printf "client: ~a\n" (utf8->string buff))

(socket:close socket-fd)
(socket:cleanup)