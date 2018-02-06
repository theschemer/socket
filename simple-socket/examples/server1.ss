(import (simple-socket socket))

(define port 8211)
(define socket-fd (socket:socket AF_INET SOCK_STREAM port))
(socket:bind socket-fd)
(socket:listen socket-fd)
(printf "listen on ~a\n" port)
(define client-fd (socket:accept socket-fd))

(socket:write client-fd "Hello World!")

(socket:close client-fd)
(socket:close socket-fd)
(socket:cleanup)


