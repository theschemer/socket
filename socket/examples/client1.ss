(import (simple-socket socket))

(define port 8211)
(define socket-fd (socket:socket AF_INET SOCK_STREAM port))
(socket:connect socket-fd)

(define buff (socket:read socket-fd))
(printf "client: ~a\n" buff)

(socket:close socket-fd)
(socket:cleanup)