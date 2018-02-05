(import (simple-socket socket-ffi))

(define socket-fd 0)
(set! socket-fd (socket AF_INET SOCK_STREAM IPPROTO_IP))
(define bind-ret (bind socket-fd AF_INET "127.0.0.1" 8200))
(define listen-ret (listen socket-fd 10))


(define clinet-fd (accept socket-fd))

(define str "Hello World!")
(c-write clinet-fd str (string-length str))


(close clinet-fd);
(close socket-fd)



