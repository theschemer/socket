(library (simple-socket socket-ffi) 
  (export
    AF_UNSPEC
    AF_UNIX
    AF_INET
    SOCK_STREAM     
    SOCK_DGRAM      
    SOCK_RAW      
    SOCK_RDM        
    SOCK_SEQPACKET
    SOCK_PACKET
    INADDR_ANY
    SOL_SOCKET
    SO_REUSEADDR
    SO_REUSEPORT
    SO_SNDBUF
    SO_RCVBUF
    SO_SNDLOWAT
    SO_RCVLOWAT
    SO_SNDTIMEO
    SO_RCVTIMEO
    SO_ERROR
    SO_TYPE
    IPPROTO_IP
    IPPROTO_TCP
    IPPROTO_UDP

    c-read
    c-write
    c-error
    close
    socket
    bind
    connect
    listen
    accept
    shutdown
    cleanup
    check)
  (import (scheme))

  (define lib-name
    (case (machine-type)
      ((arm32le) "libsocket.so")
      ((a6nt i3nt ta6nt ti3nt) "libsocket.dll")
      ((a6osx i3osx ta6osx ti3osx)  "libsocket.so")
      ((a6le i3le ta6le ti3le) "libsocket.so")
      (else "libsocket.so")))

  (define (lib-path lib-name)
    (cond
      [(file-exists? (format "./simple-socket/~a" lib-name)) (format "./simple-socket/~a" lib-name)]
      [(file-exists? (format "./lib/simple-socket/~a" lib-name)) (format "./lib/simple-socket/~a" lib-name)]
      [else lib-name]))

  (define lib (load-shared-object (lib-path lib-name)))

  (define-syntax def-function
    (syntax-rules ()
      ((_ name sym args ret)
       (define name
          (if (foreign-entry? sym)
            (foreign-procedure sym args ret)
            (lambda x (printf "error: ~a not found in ~a\n" sym lib-name)))))))


  (define AF_UNSPEC       0)  
  (define AF_UNIX         1)
  (define AF_INET         2)

  (define SOCK_STREAM      1)
  (define SOCK_DGRAM       2)
  (define SOCK_RAW         3)
  (define SOCK_RDM         4)
  (define SOCK_SEQPACKET   5)
  (define SOCK_PACKET      10)
  (define INADDR_ANY 0)

  (define SOL_SOCKET 0 )
  (define SO_REUSEADDR   #x0004)
  (define SO_REUSEPORT   #x0200)
  (define SO_SNDBUF	#x1001)
  (define SO_RCVBUF	#x1002)
  (define SO_SNDLOWAT	#x1003)
  (define SO_RCVLOWAT	#x1004)
  (define SO_SNDTIMEO	#x1005)
  (define SO_RCVTIMEO	#x1006)
  (define SO_ERROR	#x1007)
  (define SO_TYPE	#x1008)
  
  (define IPPROTO_IP 0)
  (define IPPROTO_TCP 6)
  (define IPPROTO_UDP 22)


  (def-function socket
    "_socket" (int int int) int)
  
  (def-function bind
    "_bind" (int int string int) int)

  (def-function c-read
    "_read" (int u8* size_t size_t) ssize_t)

  (def-function c-write
    "_write" (int u8* ssize_t ssize_t) ssize_t)
    
  (def-function listen
    "_listen" (int int) int)

  (def-function accept
    "_accept" (int) int)

  (def-function connect
    "_connect" (int int string int) int)
  
  (def-function close
    "_close" (int) int)

  (def-function shutdown
    "_shutdown" (int int) int)
  
  (def-function cleanup
    "_cleanup" () int)
  
  (def-function c-error
    "get_error" () string)
  
  (define check
    ; signal an error if status x is negative, using c-error to
    ; obtain the operating-system's error message
    (lambda (who x)
      (if (< x 0)
          (error who (c-error))
          x)))
)
