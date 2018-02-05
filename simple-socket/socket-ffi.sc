(library (simple-socket socket-ffi) 
  (export
   SOCK_STREAM     
   SOCK_DGRAM      
   SOCK_RAW      
   SOCK_RDM        
   SOCK_SEQPACKET
   SOCK_PACKET
   AF_INET
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

  ;  cfdopen
   c-read
   c-write
  ;  cstrlen
  ;  cwrite-all
  inet-addr
  ;  ntohl
   
   close
   socket
   bind
     connect
   listen
   accept
  ;  getsockname
  ;  getpeername
  ;  socketpair
  ;  shutdown
  ;  setsockopt
  ;  getsockopt
  ;  sendmsg
  ;  recvmsg
  ;  send
  ;  recv
  ;  sendto
  ;  recvfrom
   )
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
            (begin (printf sym) (newline) (lambda x (printf "error: ~a not found in ~a\n" sym lib-name))))))))


  (define AF_INET 2 )

  (define SOCK_STREAM      1)
  (define SOCK_DGRAM       2)
  (define SOCK_RAW         3)
  (define SOCK_RDM         4)
  (define SOCK_SEQPACKET   5)
  (define SOCK_PACKET      10)
  (define INADDR_ANY 0)

  (define IPPROTO_IP 0)
  (define IPPROTO_TCP 6)
  (define IPPROTO_UDP 22)

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


  (def-function inet-addr
    "_inet_addr" (string) int)

 

     ;;int socket(int  ,int  ,int )
  (def-function socket
    "_socket" (int int int) int)



  ;;int bind(int  ,struct sockaddr*  ,int )
  (def-function bind
    "_bind" (int int string int) int)

    ;;int listen(int  ,int )
  (def-function listen
    "_listen" (int int) int)

;;int accept(int  ,struct sockaddr*  ,socklen_t* )
(def-function accept
    "_accept" (int) int)


    (def-function c-read
  "_read" (int string int) int)

 (def-function c-write
   "_write" (int string int) int)

  (def-function close
    "_close" (int) int)

  (define c-error
    (foreign-procedure "get_error" ()
      string)) 

  ; (def-function cstrlen
  ;   "_strlen" (void*) int)


  ; (def-function cfdopen
  ;   "_fdopen" (int string) int)

  ; (def-function cread
  ;   "_read" (int void* int) int)

  ; (def-function cwrite
  ;   "_write" (int void* int) int)

  ; (def-function cwrite-all
  ;   "_write_all" (int void* int) int)
    
 

  ; ;;int connect(int  ,struct sockaddr*  ,socklen_t )
 

              (def-function connect
                "_connect" (int int string int) int)

  

  ; ;;int getsockname(int  ,struct sockaddr*  ,socklen_t* )
  ; (def-function getsockname
  ;             "_getsockname" (int void* void*) int)

  ; ;;int getpeername(int  ,struct sockaddr*  ,socklen_t* )
  ; (def-function getpeername
  ;             "_getpeername" (int void* void*) int)

  ; ;;int socketpair(int  ,int  ,int  ,int* )
  ; (def-function socketpair
  ;             "_socketpair" (int int int void*) int)

  ; ;;int shutdown(int  ,int )
  ; (def-function shutdown
  ;             "_shutdown" (int int) int)

  ; ;;int setsockopt(int  ,int  ,int  ,void*  ,socklen_t )
  ; (def-function setsockopt
  ;             "_setsockopt" (int int int void* int) int)

  ; ;;int getsockopt(int  ,int  ,int  ,void*  ,socklen_t* )
  ; (def-function getsockopt
  ;             "_getsockopt" (int int int void* void*) int)

  ; ;;int sendmsg(int  ,struct msghdr*  ,unsigned int )
  ; (def-function sendmsg
  ;             "_sendmsg" (int void* int) int)

  ; ;;int recvmsg(int  ,struct msghdr*  ,unsigned int )
  ; (def-function recvmsg
  ;             "_recvmsg" (int void* int) int)

  ; ;;ssize_t send(int  ,void*  ,size_t  ,unsigned int )
  ; (def-function send
  ;             "_send" (int void* int int) int)

  ; ;;ssize_t recv(int  ,void*  ,size_t  ,unsigned int )
  ; (def-function recv
  ;             "_recv" (int void* int int) int)

  ; ;;ssize_t sendto(int  ,void*  ,size_t  ,int  ,struct sockaddr*  ,socklen_t )
  ; (def-function sendto
  ;             "_sendto" (int void* int int void* socklen_t) int)

  ; ;;ssize_t recvfrom(int  ,void*  ,size_t  ,unsigned int  ,struct sockaddr*  ,socklen_t* )
  ; (def-function recvfrom
  ;             "_recvfrom" (int void* int int void* void*) int)

)
