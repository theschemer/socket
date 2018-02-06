(library (simple-socket socket-syntax) 
  (export
    socket:socket
    socket:bind
    socket:connect
    socket:listen
    socket:accept
    socket:write
    socket:read
    socket:close
    socket:shutdown
    socket:cleanup
    make-fd-input-port
    make-fd-output-port)
 (import (scheme) (simple-socket socket-ffi))

  (define-syntax socket:socket
    (syntax-rules ()
      [(_ family type port)
        (let ([socket-fd (check 'socket (socket family type IPPROTO_IP))])
          (list socket-fd family "127.0.0.1" port))]
      [(_ family type port addr)
        (let ([socket-fd (chekc 'socket (socket family type IPPROTO_IP))])
          (list socket-fd family addr port))]))
  
  (define-syntax socket:bind
    (syntax-rules ()
      [(_ socket)
        (check 'bind (apply bind socket))]))
  
  (define-syntax socket:connect
    (syntax-rules ()
      [(_ socket)
        (check 'connect (apply connect socket))]))

  (define-syntax socket:listen
    (syntax-rules ()
      [(_ socket)
        (check 'listen (listen (car socket) 10))]
      [(_ socket back-log)
        (check 'listen (listen (car socket) back-log))]))

 (define-syntax socket:accept
   (syntax-rules ()
     [(_ socket)
        (cons (check 'accept (accept (car socket))) (cdr socket))]))

  (define-syntax socket:write
    (syntax-rules ()
      [(_ socket msg)
        (let ([bv (string->utf8 msg)])
          (check 'c-write (c-write (car socket) bv 0 (bytevector-length bv))))]))

  (define-syntax socket:read
    (syntax-rules ()
      [(_ socket)
        (socket:read socket 1024)]
      [(_ socket len)
        (let* ([buff (make-bytevector len)]
               [n (check 'c-read (c-read (car socket) buff 0 (bytevector-length buff)))]
               [bv (make-bytevector n)])
          (bytevector-copy! buff 0 bv 0 n)
          (utf8->string bv))]))
 
  (define (socket:close socket)
    (check 'close (close (car socket))))
  
  (define (socket:shutdown socket)
    (check 'shutdown (shutdown (car socket))))

  (define (socket:cleanup)
    (check 'cleanup (cleanup)))
  
  (define (make-fd-input-port fd)
    (make-input-port (lambda (msg . args) 1) ""))
  
  (define (make-fd-output-port fd)
    (make-output-port (lambda (msg . args) 1) ""))
)
