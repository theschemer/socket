(library (simple-socket socket)
  (export
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
    close
    socket
    bind
    connect
    listen
    accept
  )
  (import 
    (scheme)
    (simple-socket socket-ffi))
)