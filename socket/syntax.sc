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

  (define nt? (string=? os "nt"))
  
  (define-syntax socket:socket
    (syntax-rules ()
      [(_ family type)
        (socket:socket family type IPPROTO_IP)]
      [(_ family type protocol)
        (begin
          (when nt?
            (let ([was (make-ftype-pointer wsadata
                          (foreign-alloc (ftype-sizeof wsadata)))])
              (wsastartup (makeword 2 2) was)))
          (check 'socket (socket family type protocol)))]))
  
  (define-syntax socket:bind
    (syntax-rules ()
      [(_ socket family ip port)
        (let* ([addr-size (ftype-sizeof sockaddr-in)]
               [serv-addr (make-ftype-pointer sockaddr-in
                            (foreign-alloc addr-size))])
          (ftype-set! sockaddr-in (sin-family) serv-addr family)
          (ftype-set! sockaddr-in (sin-addr s-addr) serv-addr (c-inet-addr ip))
          (ftype-set! sockaddr-in (sin-port) serv-addr (c-htons port))
          (socket:bind socket serv-addr addr-size))]
      [(_ socket addr size)
        (check 'bind (bind socket addr size))]))
  
  (define-syntax socket:connect
    (syntax-rules ()
      [(_ socket family ip port)
        (let* ([addr-size (ftype-sizeof sockaddr-in)]
               [serv-addr (make-ftype-pointer sockaddr-in
                            (foreign-alloc addr-size))])
          (ftype-set! sockaddr-in (sin-family) serv-addr family)
          (ftype-set! sockaddr-in (sin-addr s-addr) serv-addr (c-inet-addr ip))
          (ftype-set! sockaddr-in (sin-port) serv-addr (c-htons port))
          (socket:connect socket serv-addr addr-size))]
      [(_ socket addr size)
        (check 'connect (connect socket addr size))]))

  (define-syntax socket:listen
    (syntax-rules ()
      [(_ socket)
        (socket:listen socket 10)]
      [(_ socket back-log)
        (check 'listen (listen socket back-log))]))

  (define-syntax socket:accept
    (syntax-rules ()
      [(_ socket)
        (let* ([addr-size (ftype-sizeof sockaddr-in)]
               [clnt-addr (make-ftype-pointer sockaddr-in
                            (foreign-alloc addr-size))]
               [clnt-addr-size (make-ftype-pointer socklen-t
                            (foreign-alloc
                              (ftype-sizeof socklen-t)))])
        (ftype-set! socklen-t () clnt-addr-size addr-size)
        (socket:accept socket clnt-addr clnt-addr-size))]
      [(_ socket addr socklen)
        (check 'accept (accept socket addr socklen))]))

  (define socket:write
    (lambda (socket bv)
      (let* ([len (bytevector-length bv)])
        (check 's-write (if nt? (c-send socket bv len 0) (c-write socket bv len))))))

  (define socket:read 
    (case-lambda
      ([socket]
        (socket:read socket 1024))
      ([socket len]
        (socket:read socket len (make-bytevector 0)))
      ([socket len rbv]
        (let* ([buff (make-bytevector len)]
               [len (bytevector-length buff)]
               [n (check 's-read (if nt? (c-recv socket buff len 0) (c-read socket buff len)))]
               [bv (make-bytevector n)])
          (bytevector-copy! buff 0 bv 0 n)
          (cond
            ([= n 0] rbv)
            ([< n len] (bytevector-append rbv bv))
            (else (socket:read socket len (bytevector-append rbv bv))))))))

  (define-syntax socket:shutdown
    (syntax-rules ()
      [(_ socket howto)
        (check 'shutdown (shutdown socket howto))]))

  (define-syntax socket:close
    (syntax-rules ()
      [(_ socket)
        (check 'close ((if nt? closesocket close) socket))]))

  (define-syntax socket:cleanup
    (syntax-rules ()
      [(_)
        (check 'cleanup (if nt? (wsacleanup) 0))]))

  (define bytevector-append
    (lambda (bv1 bv2)
      (let* ([len1 (bytevector-length bv1)]
             [len2 (bytevector-length bv2)]
             [bv (make-bytevector (+ len1 len2))])
        (bytevector-copy! bv1 0 bv 0 len1)
        (bytevector-copy! bv2 0 bv len1 len2)
        bv)))
)
