(library (socket syntax) 
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
    socket:cleanup)
  (import 
    (scheme)
    (libc libc)
    (socket ffi))

  (define os
    (let ([type (machine-type)])
      (case type
        ((i3nt ti3nt a6nt ta6nt) "nt")
        ((a6osx i3osx ta6osx ti3osx)  "osx")
        ((a6le i3le ta6le ti3le) "le")
        (else (symbol->string type)))))

  (define-syntax socket:socket
    (syntax-rules ()
      [(_ family type)
        (socket:socket family type IPPROTO_IP)]
      [(_ family type protocol)
        (check 'socket (socket family type protocol))]))
  
  (define-syntax socket:bind
    (syntax-rules ()
      [(_ socket addr)
        (socket:bind socket addr (ftype-sizeof sockaddr-in))]
      [(_ socket addr size)
        (check 'bind (bind socket addr size))]))
  
  (define-syntax socket:connect
    (syntax-rules ()
      [(_ socket addr)
        (socket:connect socket addr (ftype-sizeof sockaddr-in))]
      [(_ socket addr size)
        (check 'connect (bind socket addr size))]))

  (define-syntax socket:listen
    (syntax-rules ()
      [(_ socket)
        (check 'listen (listen socket 10))]
      [(_ socket back-log)
        (check 'listen (listen socket back-log))]))

  (define-syntax socket:accept
    (syntax-rules ()
      [(_ socket)
        (check 'accept (accept (car socket))) (cdr socket))]))

       

  (define socket:write
    (lambda (socket msg)
      (let ([bv (string->utf8 msg)])
        (check 'c-write (c-write (car socket) bv 0 (bytevector-length bv))))))

  (define socket:read 
    (case-lambda
      ([socket]
        (socket:read socket 1024))
      ([socket len]
        (socket:read socket len ""))
      ([socket len msg]
        (let* ([buff (make-bytevector len)]
               [n (check 'c-read (c-read (car socket) buff 0 (bytevector-length buff)))]
               [bv (make-bytevector n)])
          (bytevector-copy! buff 0 bv 0 n)
          (cond
            ([= n 0] msg)
            ([< n len] (string-append msg (utf8->string bv)))
            (else (socket:read socket len (string-append msg (utf8->string bv)))))))))
 
  (define socket:close
    (lambda (socket)
      (check 'close (close (car socket)))))
  
  (define socket:shutdown
    (lambda (socket howto)
      (check 'shutdown (shutdown (car socket) howto))))

  (define socket:cleanup
    (lambda ()
      (check 'cleanup (cleanup))))
)
